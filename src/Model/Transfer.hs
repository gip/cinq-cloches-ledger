{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Transfer(
  Transfer(..),
  TransferState(..),
  TranferRejectionReason(..),
  Fund(..),
  Timeline(..),
  toEntity,
  fromEntity,
  toFundEntity,
  --fromFundEntity,
  FundType(..)
  ) where

import GHC.Generics
--import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Aeson.Types
import Data.Char
import Data.Maybe
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL
import Control.Monad

import DB.Common as C
import qualified DB.Schema as S
import Model.Common
import Arith

import Data.Aeson hiding (encode) -- Workaround
import Workaround (encodeWithWorkaround)
encode :: ToJSON a => a -> BL.ByteString
encode = encodeWithWorkaround

instance FromJSON TransferState where
  parseJSON (String "proposed") = return Proposed
  parseJSON (String "prepared") = return Prepared
  parseJSON (String "executed") = return Executed
  parseJSON (String "rejected") = return Rejected
  parseJSON _ = fail "could not parse TransferState"
instance ToJSON TransferState where
  toJSON s = case s of
    Proposed -> String "proposed"
    Prepared -> String "prepared"
    Executed -> String "executed"
    Rejected -> String "rejected"

instance FromJSON TranferRejectionReason where
  parseJSON (String "cancelled") = return Cancelled
  parseJSON (String "expired") = return Expired
  parseJSON _ = fail "could not parse TranferRejectionReason"
instance ToJSON TranferRejectionReason where
  toJSON Cancelled = String "cancelled"
  toJSON Expired = String "expired"

instance FromJSON FundType where
  parseJSON (String "credit") = return Credit
  parseJSON (String "debit") = return Debit
  parseJSON _ = fail "could not parse FundType"

type Condition = Text

data AdditionalInfo = AdditionalInfo {
  cases :: Maybe [Text]
} deriving (Show, Generic)
instance FromJSON AdditionalInfo
instance ToJSON AdditionalInfo

data Fund = Fund {
  account :: Maybe Text,
  amount :: Text,
  memo :: Maybe Value,
  invoice :: Maybe Text,
  authorized :: Maybe Bool
} deriving (Show, Generic)
instance FromJSON Fund
instance ToJSON Fund
-- Bug in Aeson 0.12.x, omitNothingFields doesn't work ;(
--instance ToJSON Fund where
--  toEncoding = genericToEncoding $ defaultOptions { omitNothingFields = True }

toFundEntity :: Int -> S.Key S.Transfer -> Maybe (S.Key S.Account)
                    -> FundType -> Fund -> S.Fund
toFundEntity scale transferId mAccountId type_ f =
  S.Fund transferId
         type_
         mAccountId
         (fromText scale $ amount f)
         (authorized f)

-- fromFundEntity :: Int -> S.Fund -> Fund
-- fromFundEntity scale f =
--   Fund (S.fundAccount f)
--        (toText scale $ S.fundAmount f)
--        Nothing
--        Nothing
--        (S.fundIsAuthorized f)

data Timeline = Timeline {
  proposed_at :: Maybe Text,
  prepared_at :: Maybe Text,
  executed_at :: Maybe Text,
  rejected_at :: Maybe Text
} deriving (Show, Generic)
instance FromJSON Timeline
instance ToJSON Timeline
defaultTimeline = Timeline Nothing Nothing Nothing Nothing

data Transfer = Transfer {
  id :: Uuid,
  ledger :: Text,
  state :: Maybe TransferState,
  rejection_reason :: Maybe TranferRejectionReason,
  execution_condition :: Maybe Condition,
  cancellation_condition :: Maybe Condition,
  expiry_duration :: Maybe Text,
  credits :: [Fund],
  debits :: [Fund],
  additional_info :: Maybe AdditionalInfo,
  timeline :: Maybe Timeline
} deriving (Show, Generic)
instance FromJSON Transfer
instance ToJSON Transfer
-- Bug in Aeson 0.12.x, omitNothingFields doesn't work ;(
--instance ToJSON Transfer where
--  toEncoding = genericToEncoding $ defaultOptions { omitNothingFields = True }

justOr def (Just v) = v
justOr def _ = def

toEntity :: Transfer -> S.Transfer
toEntity t =
  S.Transfer (uuidToText $ Model.Transfer.id t)
             (ledger t)
             (justOr Proposed $ state t)
             (rejection_reason t)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (liftM (decodeUtf8 . BL.toStrict . encode) $
               additional_info t)
            (execution_condition t)
            (cancellation_condition t)
            (liftM (read . unpack) $ expiry_duration t)
            (decodeUtf8 . BL.toStrict . encode $ credits t)
            (decodeUtf8 . BL.toStrict . encode $ debits t)
            Nothing

fromEntity :: S.Transfer -> Transfer
fromEntity t =
  Transfer (Uuid $ S.transferUuid t)
           (S.transferLedger t)
           (Just $ S.transferState t)
           (S.transferRejectionReason t)
           (S.transferExecutionCondition t)
           (S.transferCancellationCondition t)
           (liftM (pack . show) $ S.transferExpiryDuration t)
           (fromJust $ decode . BL.fromStrict . encodeUtf8 $ S.transferCredits t)
           (fromJust $ decode . BL.fromStrict . encodeUtf8 $ S.transferDebits t)
           (S.transferAdditionalInfo t >>= decode . BL.fromStrict . encodeUtf8)
           (Just timeline)
  where
    show' :: Maybe UTCTime -> Maybe Text
    show' = liftM (pack . show)
    timeline =
      Timeline (show' $ S.transferProposedAt t)
               (show' $ S.transferPreparedAt t)
               (show' $ S.transferExecutedAt t)
               (show' $ S.transferRejectedAt t)

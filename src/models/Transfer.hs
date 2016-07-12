{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.Transfer(
  Transfer(..),
  TransferState(..),
  TranferRejectionReason(..),
  toEntity
  ) where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL
import Control.Monad

import DB.Common as C
import qualified DB.Schema as S
import Models.Common

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

type Condition = String

data AdditionalInfo = AdditionalInfo {
  cases :: Maybe [Text]
} deriving (Show, Generic)
instance FromJSON AdditionalInfo
instance ToJSON AdditionalInfo

data Funds = Funds {
  account :: Maybe Text,
  amount :: Text,
  memo :: Maybe Value,
  invoice :: Maybe Text,
  authorized :: Maybe Bool
} deriving (Show, Generic)
instance FromJSON Funds

data Transfer = Transfer {
  id :: Uuid,
  ledger :: Text,
  state :: TransferState,
  rejection_reason :: Maybe TranferRejectionReason,
  execution_condition :: Maybe Condition,
  cancellation_condition :: Maybe Condition,
  expiry_duration :: Maybe Text,
  credits :: [Funds],
  debits :: [Funds],
  additional_info :: Maybe AdditionalInfo
} deriving (Show, Generic)
instance FromJSON Transfer

justOr def (Just v) = v
justOr def _ = def

toEntity :: Transfer -> S.Transfer
toEntity t =
  S.Transfer (uuidToText $ Models.Transfer.id t)
             (ledger t)
             (state t)
             (rejection_reason t)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (liftM (decodeUtf8 . BL.toStrict . encode) $
               additional_info t)

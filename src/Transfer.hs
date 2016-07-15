{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
module Transfer(
  http,
  httpFulfill,
  rejectTransfer,
  notifyTransfer
  ) where

import Network.Wai
import Network.HTTP.Types
import Database.Persist
import Database.Persist.Sql
import Data.Time.Clock
--import Data.Aeson -- Workaround
import Data.Either
import Data.Maybe
import Data.Typeable
import qualified Data.List as L
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.ByteString.Lazy (fromStrict, toStrict, ByteString)
import Data.Text.Encoding
import Data.Text as T
import qualified Data.Vault.Lazy as V

import Ledger
import Model.Transfer as M
import Model.Notification
import Model.Common as C
import DB.Schema as S
import Fund
import Http
import Arith
import Notification
import Account (extractAccountName)
import Crypto.Condition

import Data.Aeson hiding (encode) -- Workaround
import Workaround (encodeWithWorkaround)
encode :: ToJSON a => a -> ByteString
encode = encodeWithWorkaround

http ledger uriParams req respond = doit `catch` (respond . caught)
  where
    doit = do
      print $ join $ V.lookup (keyAuth ledger) (vault req)
      let Just transferId = lookup "id" uriParams
      let method = requestMethod req
      if | method == methodPut -> put ledger transferId req respond
         | otherwise -> throw UnknownMethod

httpFulfill ledger uriParams req respond = doit `catch` (respond . caught)
  where
    doit = do
      print $ join $ V.lookup (keyAuth ledger) (vault req)
      let Just transferId = lookup "id" uriParams
      let method = requestMethod req
      if | method == methodPut -> putFulfill ledger transferId req respond
         | otherwise -> throw UnknownMethod

put ledger transferId req respond = do
  rawBody <- strictRequestBody req
  let maybeTransfer = decode rawBody :: Maybe M.Transfer
  case maybeTransfer of
    Nothing -> throw (WrongFormat "transfer")
    Just transfer -> do
      when ((C.Uuid $ decodeUtf8 transferId) /= M.id transfer) $
        throw (WrongValue "transfer id")
      r <- runDB ledger $ do
          t <- liftIO getCurrentTime
          let mExpires = liftM (\duration -> addUTCTime (fromIntegral $ read $ unpack duration) t) (expiry_duration transfer)
          -- We insert the transfer now
          mTransferK <- insertBy $ (toEntity transfer) { transferProposedAt = Just t,
                                                         transferExpiresAt = mExpires }
          case mTransferK of
            Left _ -> throw (AlreadExists "transfer")
            Right transferK -> do
              creditVL <- mapM (insertFund transferK Credit) (credits transfer)
              debitVL <- mapM (insertFund transferK Debit) (debits transfer)
              return (transferK, creditVL, debitVL)
      processFromProposed ledger transfer r respond
  where
    scale = amountScale ledger
    -- insertFund inserts rows for credits/debits involving accounts
    --   on THIS ledger only
    insertFund transferK typ fund = do
      mAccountK <- case liftM (extractAccountName ledger) $ account fund of
        Nothing -> return Nothing -- not on this ledger, skip it
        Just n -> do
          r <- selectFirst [AccountName ==. n] []
          case r of Just e -> return $ Just (entityKey e)
                    _ -> throw $ UnrecognizedAccount (fromStrict . encodeUtf8 $ n)
      let fundV = toFundEntity scale transferK mAccountK typ fund
      insert fundV
      return fundV

putFulfill ledger transferId req respond = do
  fulfillment <- strictRequestBody req
  processFromPrepared ledger (decodeUtf8 transferId) (toStrict fulfillment) respond

processFromPrepared ledger transferId fulfillment respond = do
  (body, mNotifyTransferV) <- runDB ledger $ do
    rawExecute "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE" [] -- TODO: check this
    mTransferE <- selectFirst [TransferUuid ==. transferId] []
    case mTransferE of
      Nothing -> throw $ NotFound "transfer"
      Just transferE -> do
        let transferK = entityKey transferE
        let transferV = entityVal transferE
        let currentState = transferState transferV
        let cURI = decodeUtf8 $ conditionToURI $ fulfillmentCondition $ fulfillmentFromURI fulfillment
        let cType = if | transferExecutionCondition transferV == Just cURI -> Executed
                       | transferCancellationCondition transferV == Just cURI -> Rejected
                       | otherwise -> throw UnmetCondition
        if | cType == Executed && currentState == Executed -> do
                        -- transfer already executed
                        return ("", Nothing) -- TODO: return transfer
           | cType == Rejected && currentState == Rejected -> do
                        -- transfer already rejected
                        return ("", Nothing) -- TODO: return transfer
           | cType == Executed && currentState == Prepared -> do
                        creditEL <- selectList [FundTransferId ==. transferK,
                                                FundType ==. Credit] []
                        mapM (applyCredit ledger . entityVal) creditEL
                        t <- liftIO getCurrentTime
                        update transferK [TransferState =. Executed,
                                          TransferExecutedAt =. Just t,
                                          TransferFulfillment =. Just fulfillmentText]
                        transferV' <- getJust transferK
                        return ("", Just transferV')
           | cType == Rejected && (currentState == Proposed ||
                                   currentState == Prepared) -> do
                        rejectTransfer ledger Cancelled transferK (Just fulfillmentText)
                        transferV' <- getJust transferK
                        return ("", Just transferV')
           | otherwise -> throw InvalidFulfillment
  case mNotifyTransferV of
    Just transferV -> notifyTransfer ledger transferV
    Nothing -> return ()
  respond $ responseLBS status200 [] body -- TODO: change this
  where
    fulfillmentText = decodeUtf8 fulfillment

rejectTransfer ledger reason transferK mFulfillment = do
  debitEL <- selectList [FundTransferId ==. transferK,
                         FundType ==. Debit] []
  liftIO $ print debitEL
  mapM (unapplyDebit ledger . entityVal) debitEL
  t <- liftIO getCurrentTime
  update transferK [TransferState =. Rejected,
                    TransferRejectedAt =. Just t,
                    TransferRejectionReason =. Just reason,
                    TransferFulfillment =. mFulfillment]

processFromProposed ledger transfer (transferK, creditVL, debitVL) respond = do
  transferV <- runDB ledger $ do
    mapM (applyDebit ledger) debitVL
    t <- liftIO getCurrentTime
    update transferK [TransferState =. Prepared, TransferPreparedAt =. Just t]
    getJust transferK
  notifyTransfer ledger transferV
  respond $ responseLBS status200 [] "" -- TODO: change this

notifyTransfer ledger transferV = do
  let notification = Notification (fromEntity transferV) rResource
  let message = encode notification
  mapM_ (notify ledger message) accountL
  where
    decodeText :: Text -> Maybe [M.Fund]
    decodeText = decode . fromStrict . encodeUtf8
    fundL :: [M.Fund]
    fundL = (join $ maybeToList (decodeText $ transferCredits transferV)) ++
            (join $ maybeToList (decodeText $ transferDebits transferV))
    accountL = L.map (extractAccountName ledger) (catMaybes $ L.map account fundL)
    state = transferState transferV
    rejectReason = transferRejectionReason transferV
    fulfillment = transferFulfillment transferV
    rResource = if | state == Rejected && rejectReason == Just Cancelled
                        -> Just (RelatedResource Nothing fulfillment)
                   | state == Executed
                        -> Just (RelatedResource fulfillment Nothing)
                   | otherwise -> Nothing

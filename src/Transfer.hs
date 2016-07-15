{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
module Transfer(http, httpFulfill, rejectTransfer) where

import Network.Wai
import Network.HTTP.Types
import Database.Persist
import Database.Persist.Sql
import Data.Time.Clock
import Data.Aeson
import Data.Either
import Data.Typeable
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text.Encoding
import Data.Text as T
import qualified Data.Vault.Lazy as V
--import qualified Data.ByteString.Lazy as BL

import Ledger
import Models.Transfer as M
import Models.Common as C
import DB.Schema as S
import Fund
import Http
import Arith
import Crypto.Condition

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
    insertFund transferK typ fund = do
      mAccountK <- case liftM extract $ account fund of
        Nothing -> return Nothing
        Just n -> do
          r <- selectFirst [AccountName ==. n] []
          case r of Just e -> return $ Just (entityKey e)
                    _ -> throw $ UnrecognizedAccount (fromStrict . encodeUtf8 $ n)
      let fundV = toFundEntity scale transferK mAccountK typ fund
      insert fundV
      return fundV
      where extract m =
              case commonPrefixes (T.concat [baseUri ledger, "/accounts/"]) m of
                Just (_, "", n) -> n
                _ -> throw $ UnrecognizedAccount (fromStrict . encodeUtf8 $ m)

putFulfill ledger transferId req respond = do
  fulfillment <- strictRequestBody req
  processFromPrepared ledger (decodeUtf8 transferId) (toStrict fulfillment) respond

processFromPrepared ledger transferId fulfillment respond = do
  body <- runDB ledger $ do
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
                        return "" -- TODO: return transfer
           | cType == Rejected && currentState == Rejected -> do
                        -- transfer already rejected
                        return "" -- TODO: return transfer
           | cType == Executed && currentState == Prepared -> do
                        creditEL <- selectList [FundTransferId ==. transferK,
                                                FundType ==. Credit] []
                        mapM (applyCredit ledger . entityVal) creditEL
                        updateState transferK Executed
                        return "" -- TODO: Execute transfer
           | cType == Rejected && (currentState == Proposed ||
                                   currentState == Prepared) -> do
                        rejectTransfer ledger Cancelled transferK
                        return "" -- TODO: reject transfer
           | otherwise -> throw InvalidFulfillment
  respond $ responseLBS status200 [] body -- TODO: change this

rejectTransfer ledger reason transferK = do
  debitEL <- selectList [FundTransferId ==. transferK,
                         FundType ==. Debit] []
  liftIO $ print debitEL
  mapM (unapplyDebit ledger . entityVal) debitEL
  t <- liftIO getCurrentTime
  update transferK [TransferState =. Rejected,
                    TransferRejectedAt =. Just t,
                    TransferRejectionReason =. Just reason]

processFromProposed ledger transfer (transferK, creditVL, debitVL) respond = do
  runDB ledger $ do
    mapM (applyDebit ledger) debitVL
    t <- liftIO getCurrentTime
    update transferK [TransferState =. Prepared, TransferPreparedAt =. Just t]
  respond $ responseLBS status200 [] "" -- TODO: change this

updateState transferK state = do
  t <- liftIO getCurrentTime
  update transferK [TransferState =. state, timeCtor =. Just t]
  where
    timeCtor = case state of
      Prepared -> TransferPreparedAt
      Proposed -> TransferProposedAt
      Executed -> TransferExecutedAt
      Rejected -> TransferRejectedAt

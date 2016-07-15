{-# LANGUAGE FlexibleContexts #-}
module Monitor where

import Database.Persist
import Control.Concurrent
import Data.Time.Clock

import Ledger
import Model.Transfer as M
import DB.Common
import DB.Schema
import Transfer
import Http

expiryMonitorThread ledger = do
  t <- getCurrentTime
  expiredTransferEL <- runDB ledger $
    selectList ([TransferExpiresAt <=. Just t] ++ ([TransferState ==. Proposed] ||. [TransferState ==. Prepared])) []
  mapM rejectAndNotify expiredTransferEL
  threadDelay $ (monitorInterval ledger) * 1000000
  expiryMonitorThread ledger
  where
    rejectAndNotify e = do
      newV <- runDB ledger $ do
        rejectTransfer ledger Expired (entityKey e) Nothing
        getJust (entityKey e)
      notifyTransfer ledger newV

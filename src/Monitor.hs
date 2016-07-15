module Monitor where

import Database.Persist
import Control.Concurrent
import Data.Time.Clock

import Ledger
import Models.Transfer as M
import DB.Common
import DB.Schema
import Transfer
import Http

expiryMonitorThread ledger = do
  t <- getCurrentTime
  expiredTransferEL <- runDB ledger $
    selectList ([TransferExpiresAt <=. Just t] ++ ([TransferState ==. Proposed] ||. [TransferState ==. Prepared])) []
  mapM (\e -> runDB ledger $ rejectTransfer ledger Expired (entityKey e)) expiredTransferEL
  threadDelay $ (monitorInterval ledger) * 1000000
  expiryMonitorThread ledger

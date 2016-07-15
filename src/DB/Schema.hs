{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module DB.Schema where
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Data.Time.Clock
import Data.Int (Int64)
import Data.Text (Text)

import DB.Common

-- export Key
type Key = Database.Persist.Sql.Key

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Transfer
  uuid Text
  Uuid uuid
  ledger Text
  state TransferState
  rejectionReason TranferRejectionReason Maybe
  expiresAt UTCTime Maybe
  proposedAt UTCTime Maybe
  preparedAt UTCTime Maybe
  executedAt UTCTime Maybe
  rejectedAt UTCTime Maybe
  additionalInfo Text Maybe
  executionCondition Text Maybe
  cancellationCondition Text Maybe
  expiryDuration Int Maybe
  credits Text
  debits Text
  fulfillment Text Maybe
  deriving Show

Fund
  transferId TransferId
  type FundType
  account AccountId Maybe
  amount Int64
  isAuthorized Bool Maybe
  deriving Show

Fulfillment
  transferId TransferId
  conditionFulfillment Text
  deriving Show

Account
  name Text
  Name name
  balance Int64
  connector Text Maybe
  passwordHash Text Maybe
  publicKey Text Maybe
  isAdmin Bool
  isDisabled Bool
  fingerprint Text Maybe
  minAllowedBalance Int64 default=0
|]

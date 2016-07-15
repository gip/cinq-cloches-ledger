{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module DB.Common where

import GHC.Generics
import Database.Persist.TH

data TransferState = Proposed | Prepared | Executed | Rejected
  deriving (Show, Read, Eq, Generic)
derivePersistField "TransferState"

data TranferRejectionReason = Cancelled | Expired
  deriving (Show, Read, Eq, Generic)
derivePersistField "TranferRejectionReason"

data FundType = Credit | Debit
  deriving (Show, Read, Generic)
derivePersistField "FundType"

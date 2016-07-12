{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module DB.Common where

import GHC.Generics
import Database.Persist.TH

data TransferState = Proposed | Prepared | Executed | Rejected
  deriving (Show, Read, Generic)
derivePersistField "TransferState"

-- instance FromJSON TransferState where
--   parseJSON (String "proposed") = return Proposed
--   parseJSON (String "prepared") = return Prepared
--   parseJSON (String "executed") = return Executed
--   parseJSON (String "rejected") = return Rejected
--   parseJSON _ = fail "could not parse TransferState"
-- instance ToJSON TransferState where
--   toJSON s = case s of
--     Proposed -> String "proposed"
--     Prepared -> String "prepared"
--     Executed -> String "executed"
--     Rejected -> String "rejected"

data TranferRejectionReason = Cancelled | Expired
  deriving (Show, Read, Generic)
derivePersistField "TranferRejectionReason"

-- instance FromJSON TranferRejectionReason where
--   parseJSON (String "cancelled") = return Cancelled
--   parseJSON (String "expired") = return Expired
--   parseJSON _ = fail "could not parse TranferRejectionReason"

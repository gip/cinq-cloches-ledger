{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
module Health(http) where

import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
import GHC.Generics
import Data.Text
import Data.Aeson

data Health = Health {
  status :: Text
} deriving (Generic, Show)
instance ToJSON Health

http ledger _ _ respond =
  respond $
    responseLBS
    status200
    [(hContentType, "application/json")]
    (encode $ Health "OK")

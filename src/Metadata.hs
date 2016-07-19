{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
module Metadata(http) where

import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
import GHC.Generics
import Data.Text as T
import Data.Aeson

import Ledger

data Endpoint = Endpoint {
  health :: Text
} deriving (Generic, Show)
instance ToJSON Endpoint

data Meta = Meta {
  urls :: Endpoint,
  scale :: Int,
  precision :: Int
} deriving (Generic, Show)
instance ToJSON Meta

http ledger _ _ respond =
  respond $
    responseLBS
    status200
    [(hContentType, "application/json")]
    (encode meta)
  where
    endpoint = Endpoint (T.concat [baseUri ledger, "/health"])
    meta = Meta endpoint (amountScale ledger) (18 - amountScale ledger)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Connector(http) where

import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types
import Network.WebSockets

import GHC.Generics
import Database.Persist hiding (get)
import Data.Aeson
import qualified Data.Text as T
import Data.Maybe
import Data.Typeable
import Control.Monad
import Control.Exception
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vault.Lazy as V

import Ledger as L
import Model.Account as M
import DB.Schema as S
import Http

data Connector = Connector {
  id :: T.Text,
  name :: T.Text,
  connector :: T.Text
} deriving (Show, Generic)
instance ToJSON Connector

http ledger uriParams req respond = doit `catch` (respond . caught)
  where
    doit = do
      let method = requestMethod req
      if | method == methodGet -> get ledger req respond
         | otherwise -> throw UnknownMethod

get ledger req respond = do
  list <- runDB ledger $ selectList [AccountConnector !=. Nothing] []
  respond $ responseLBS status200
              standardHeaders
              (encode $ map (build . entityVal) list)
  where build e = Connector uri name (fromJust $ accountConnector e)
          where name = accountName e
                uri = T.concat [baseUri ledger, "/accounts/", name]

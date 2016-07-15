{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
module Account(http, ws, createAccount) where

import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types
import Network.WebSockets

import Database.Persist hiding (get)
import Data.Aeson
import Data.Text
import Data.Either
import Data.Typeable
import Control.Monad
import Control.Exception
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vault.Lazy as V

import Ledger as L
import Models.Account as M
import DB.Schema as S
import Http

createAccount ledger user pass admin = do
  r <- runDB ledger $ insertBy $ toEntity (amountScale ledger) account
  return $ case r of Right k -> k
                     Left e -> entityKey e
  where account = defaultAccount { name = user,
                                   password = pass,
                                   M.isAdmin = Just admin }

-- TODO: use ErrorT
http ledger uriParams req respond = doit `catch` (respond . caught)
  where
    authorized =
      case join $ V.lookup (keyAuth ledger) (vault req) of
      Nothing -> False
      Just auth -> L.isAdmin auth
    doit = do
      when (not authorized) $ throw NotAutorized
      let Just accountName = lookup "id" uriParams
      let method = requestMethod req
      if | method == methodPut -> put ledger accountName req respond
         | method == methodGet -> get ledger accountName req respond
         | otherwise -> throw UnknownMethod

put ledger accountName req respond = do
  rawBody <- strictRequestBody req
  let maybeTransfer = decode rawBody :: Maybe M.Account
  case maybeTransfer of
    Nothing -> throw (WrongFormat "transfer")
    Just account -> do
      when (decodeUtf8 accountName /= name account) $
        throw (WrongValue "account name")
      r <- runDB ledger $ insertBy $ toEntity (amountScale ledger) account
      when (isLeft r) $ throw (AlreadExists "account")
  respond $ responseLBS status200 [] ""

get ledger accountName req respond = do
  r <- runDB ledger $ selectFirst [AccountName ==. decodeUtf8 accountName] []
  case r of Just entity ->
              respond $
              responseLBS status200 standardHeaders (encode . fromEntity scale $ entityVal entity)
            _ -> throw (NotFound "account")
  where scale = amountScale ledger

ws ledger _ = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        sendTextData conn ("Hello, client!" :: Text)
    backupApp :: Application
    backupApp _ respond =
      respond $ responseLBS status400 [] "Not a WebSocket request"

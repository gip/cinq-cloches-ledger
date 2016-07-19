{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
module Account(http, createAccount, extractAccountName) where

import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types
import Network.WebSockets

import Database.Persist hiding (get)
--import Data.Aeson
import Data.Text as T
import Data.Either
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

import Data.Aeson hiding (encode) -- Workaround
import Workaround (encodeWithWorkaround)
encode :: ToJSON a => a -> BL.ByteString
encode = encodeWithWorkaround

-- Account name from accountURI or throw an error if not on this ledger
extractAccountName ledger m =
  case commonPrefixes (T.concat [baseUri ledger, "/accounts/"]) m of
    Just (_, "", n) -> n
    _ -> throw $ UnrecognizedAccount (BL.fromStrict . encodeUtf8 $ m)

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
    Just accountName = lookup "id" uriParams
    authorized =
      case join $ V.lookup (keyAuth ledger) (vault req) of
      Nothing -> False
      Just auth -> L.isAdmin auth || (decodeUtf8 accountName) == (L.user auth)
    doit = do
      let method = requestMethod req
      if | method == methodPut -> do
           when (not authorized) $ throw NotAutorized
           put ledger accountName req respond
         | method == methodGet -> get ledger accountName req respond authorized
         | otherwise -> throw UnknownMethod

put ledger accountName req respond = do
  rawBody <- strictRequestBody req
  let maybeTransfer = decode rawBody :: Maybe M.Account
  status <- case maybeTransfer of
    Nothing -> throw (WrongFormat "transfer")
    Just account -> do
      when (decodeUtf8 accountName /= name account) $
        throw (WrongValue "account name")
      r <- runDB ledger $ insertBy $ toEntity (amountScale ledger) account
      --when (isLeft r) $ throw (AlreadExists "account")
      --return status200
      return $ if isLeft r then status200 else status201
  respond $ responseLBS status [] ""

get ledger accountName req respond authorized = do
  r <- runDB ledger $ selectFirst [AccountName ==. decodeUtf8 accountName] []
  case r of Just entity -> do
              let fullAccount = fromEntity scale (baseUri ledger) $ entityVal entity
              let partialAccount = defaultAccount { name = name fullAccount,
                                                    M.id = M.id fullAccount }
              let account = if authorized then fullAccount else partialAccount
              respond $ responseLBS status200 standardHeaders $ encode account
            _ -> throw (NotFound "account")
  where scale = amountScale ledger

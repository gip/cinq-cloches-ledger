{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
module Transfer(http) where

import Network.Wai
import Network.HTTP.Types
import Database.Persist
import Data.Aeson
import Data.Either
import Data.Typeable
import Control.Monad
import Control.Exception
import Data.Text.Encoding
import qualified Data.Vault.Lazy as V
--import qualified Data.ByteString.Lazy as BL

import Ledger
import Models.Transfer as M
import Models.Common as C
import DB.Schema as S
import Http


http ledger uriParams req respond = doit `catch` (respond . caught)
  where
    doit = do
      -- do some auth maybe?
      print $ join $ V.lookup (keyAuth ledger) (vault req)
      let Just transferId = lookup "id" uriParams
      let method = requestMethod req
      if | method == methodPut -> putTransfer (runDB ledger) transferId req respond
         | otherwise -> throw UnknownMethod

putTransfer runDB transferId req respond = do
  rawBody <- strictRequestBody req
  let maybeTransfer = decode rawBody :: Maybe M.Transfer
  case maybeTransfer of
    Nothing -> throw (WrongFormat "transfer")
    Just transfer -> do
      when ((C.Uuid $ decodeUtf8 transferId) /= M.id transfer) $
        throw (WrongValue "transfer id")
      r <- runDB $ insertBy $ toEntity transfer
      when (isLeft r) $ throw (AlreadExists "transfer")
  respond $ responseLBS status200 [] ""

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Notification(ws, notify) where

import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types
import Network.WebSockets

import Database.Persist hiding (get)
import Data.Aeson
import Data.Text hiding (filter)
import Data.Either
import Data.Maybe
import Data.Typeable
import Data.List as L
import qualified Data.Map.Strict as MS
import Control.Monad
import Control.Exception
import Control.Concurrent.STM
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vault.Lazy as V

import Ledger
import Model.Account as M
import Model.Notification
import DB.Schema as S
import Http

ws ledger uriParams req respond = do
  when (not authorized) $ throw NotAutorized
  websocketsOr defaultConnectionOptions (wsApp ledger accountName) backupApp req respond
  where
    accountName = decodeUtf8 $ fromJust $ lookup "id" uriParams
    authorized =
      case join $ V.lookup (keyAuth ledger) (vault req) of
        Nothing -> False
        Just auth ->
          case lookup "id" uriParams of
            Just "*" -> Ledger.isAdmin auth
            Just name -> decodeUtf8 name == Ledger.user auth
    backupApp :: Application
    backupApp _ respond =
      respond $ responseLBS status400 [] "not a websocket request"


wsApp :: Ledger -> Text -> ServerApp
wsApp ledger accountName pending_conn = do
  conn <- acceptRequest pending_conn
  -- we are connected
  chan <- atomically $ do
    chan <- newTChan
    m <- readTVar $ listeners ledger
    let conns' = case MS.lookup accountName m of
                        Nothing -> [(chan, conn)]
                        Just conns -> (chan, conn) : conns
    writeTVar (listeners ledger) $ MS.insert accountName conns' m
    return chan
  putStrLn $ ">>> websocket connection for account: " ++ unpack accountName
  catch (wsWorker ledger conn chan) $ caught chan
  where
    caught chan (e :: ConnectionException) = do
      -- we have disconnected
      putStrLn $ ">>> websocket disconnected for account: " ++ unpack accountName
      atomically $ do
        m <- readTVar $ listeners ledger
        let conns' = case MS.lookup accountName m of
                         Nothing -> [] -- this should really never happen!
                         Just conns -> filter (\(c,_) -> c /= chan) conns
        writeTVar (listeners ledger) $ MS.insert accountName conns' m

wsWorker ledger conn chan = do
  msg <- atomically $ readTChan chan
  sendTextData conn msg
  wsWorker ledger conn chan

notify ledger message accountName = do
  m <- atomically $ readTVar (listeners ledger)
  let chans = L.map fst $ fromMaybe [] (MS.lookup accountName m)
  let chansStar = L.map fst $ fromMaybe [] (MS.lookup "*" m)
  mapM (\chan -> atomically $ writeTChan chan message) $ L.nub (chans ++ chansStar)

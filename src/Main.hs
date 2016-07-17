{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Text
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Logger (runLoggingT, runStdoutLoggingT)
import Network.WebSockets
import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Route
import Network.Wai.Handler.WebSockets
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Database.Persist.Postgresql

import Ledger
import Transfer
import Account
import Notification
import Health
import Connector
import Auth
import Monitor
import DB.Schema
import Workaround


-- Default endpoint
appSlash _ _ respond = do
    respond $ responseLBS
        status200
        []
        "Cinq Cloches Legder"

app ledger = route $
                 ("/", appSlash)
               : ("/health", Health.http ledger)
               -- transfer routes
               : ("/transfers/:id", Transfer.http ledger)
               : ("/transfers/:id/fulfillment", Transfer.httpFulfill ledger)
               : ("/transfers/:id/state", appSlash)
               : ("/transfers/byExecutionCondition/:id", Transfer.httpCondition ledger)
               -- connectors
               : ("/connectors", Connector.http ledger)
               -- accounts
               : ("/accounts/:id", Account.http ledger)
               -- notifications (websocket)
               : ("/accounts/:id/transfers", Notification.ws ledger)
               : []

main :: IO ()
main = do
  ledger <- createLedger
  runDB ledger $ runMigration migrateAll
  createAccount ledger (adminName ledger) (Just $ adminPassword ledger) True
  holdK <- createAccount ledger "hold" Nothing False
  let ledger' = ledger { holdAccountK = holdK }
  forkIO $ expiryMonitorThread ledger'
  run (port ledger') $ logStdoutDev $ basicAuth ledger' $ (app ledger')

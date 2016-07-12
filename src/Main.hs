{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Text
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
import Health
import Auth
import DB.Schema

--app :: Application
appSlash _ _ respond = do
    respond $ responseLBS
        status200
        []
        "Cinq Cloches Legder (WIP)"

--topApp :: Application
app ledger = route $
                 ("/", appSlash)
               : ("/health", Health.http ledger)
               -- transfer routes
               : ("/transfers/:id", Transfer.http ledger)
               : ("/transfers/:id/fulfillment", appSlash)
               : ("/transfers/:id/state", appSlash)
               -- connectors
               : ("/connectors", appSlash)
               -- accounts
               : ("/accounts/:name", Account.http ledger)
               -- notifications (websocket)
               : ("/accounts/:name/transfers", Account.ws ledger)
               : []

main :: IO ()
main = do
  ledger <- createLedger
  runDB ledger $ runMigration migrateAll
  createAdminAccount ledger (adminName ledger) (Just $ adminPassword ledger)
  --run (port ledger) $ logStdoutDev (app ledger)
  run (port ledger) $ logStdoutDev $ basicAuth ledger $ (app ledger)
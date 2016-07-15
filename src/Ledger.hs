{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ledger where

import System.Environment
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Exception
import Control.Monad.Logger (runLoggingT, runStdoutLoggingT)
import Data.Typeable
import Data.Maybe
import Data.ByteString.Char8 as B8
import Data.Text as T
import qualified Data.Vault.Lazy as V

-- Postgres
import qualified Database.Persist.Postgresql as PG

import DB.Schema as S

data ConfigurationError =
    ConfigurationMissing String
  | ConfigurationValueNotParsed String
  deriving (Show, Typeable)
instance Exception ConfigurationError

data Auth = Auth {
  user :: Text,
  isAdmin :: Bool
} deriving (Show)

authIsAdmin :: Monad m => m Auth -> m Bool
authIsAdmin = liftM isAdmin

data Ledger = Ledger {
  amountScale :: !Int,
  port :: !Int,
  keyAuth :: V.Key (Maybe Auth),
  adminName :: !Text,
  adminPassword :: !Text,
  baseUri :: !Text,
  holdAccountK :: S.Key S.Account,
  monitorInterval :: !Int,
  runDB :: forall a m. (MonadBaseControl IO m, MonadIO m) => PG.SqlPersistT m a -> m a
}

--parse :: Read a => [(String, String)] -> String -> a
parse env var =
  case lookup var env of
    Nothing -> throw $ ConfigurationMissing var
    Just s ->
      case listToMaybe $ reads s of
        Just (a, "") -> a
        _ -> throw $ ConfigurationValueNotParsed var

parseString env var =
  case lookup var env of
    Nothing -> throw $ ConfigurationMissing var
    Just s -> s

createLedger :: IO Ledger
createLedger = do
  env <- getEnvironment
  let scale = parse env "LEDGER_AMOUNT_SCALE"
  let port = parse env "LEDGER_PORT"
  let conn = B8.pack $ parseString env "LEDGER_DB_CONNECTION_STRING"
  let aName = parseString env "LEDGER_ADMIN_USER"
  let aPass = parseString env "LEDGER_ADMIN_PASSWORD"
  let baseUri = parseString env "LEDGER_BASE_URI"
  let mInterval = parse env "LEDGER_MONITOR_INTERVAL"
  pool <- runStdoutLoggingT $ PG.createPostgresqlPool conn 10
  keyAuth <- V.newKey
  return $ Ledger scale
                  port
                  keyAuth
                  (T.pack aName)
                  (T.pack aPass)
                  (T.pack baseUri)
                  undefined
                  mInterval
                  (\action -> PG.runSqlPool action pool)

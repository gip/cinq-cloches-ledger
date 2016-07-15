{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Fund(
  applyDebit,
  unapplyDebit,
  applyCredit,
  unapplyCredit
) where

import Network.Wai
import Network.HTTP.Types
import Database.Persist
import Database.Persist.Sql
import Data.Aeson
import Data.Either
import Data.Typeable
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Exception
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text.Encoding
import Data.Text as T
import qualified Data.Vault.Lazy as V

import Ledger
import Models.Transfer as M
import Models.Common as C
import DB.Schema as S
import Http
import Arith

unwrap e Nothing = throw e
unwrap e (Just v) = v

applyFund isDebit ledger debitV = do
  let accountK = unwrap DebitWithoutAccount (fundAccount debitV)
  mAccountV <- get (accountK :: S.Key S.Account)
  let accountV = unwrap (DBLogicError "referenced account not found") mAccountV
  liftIO $ print (holdAccountK ledger)
  mHoldV <- get (holdAccountK ledger)
  let holdV = unwrap (DBLogicError "hold account not found") mHoldV
  -- Now let's apply the debit...
  let amount = fundAmount debitV
  let originalBalance = accountBalance accountV
  let originalHoldBalance = accountBalance holdV
  let newBalance = opAccount originalBalance amount
  let newHoldBalance = opHold originalHoldBalance amount
  when (newBalance < accountMinAllowedBalance accountV) $
    throw NoSufficientFunds
  update accountK [AccountBalance =. newBalance]
  update (holdAccountK ledger) [AccountBalance =. newHoldBalance]
  -- TODO: send notification
  -- TODO: if not atomic we should advance to next step immeditely
  return ()
  where (opAccount, opHold) = if isDebit then (sub, add) else (add, sub)

type ApplyFund =
  forall m. (MonadIO m) => Ledger -> S.Fund -> ReaderT SqlBackend m ()

applyDebit :: ApplyFund
applyDebit = applyFund True

unapplyDebit ::ApplyFund
unapplyDebit = applyFund False

applyCredit :: ApplyFund
applyCredit = applyFund False

unapplyCredit :: ApplyFund
unapplyCredit = applyFund True

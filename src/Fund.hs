{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Fund(
  applyDebit,
  unapplyDebit,
  applyCredit,
  commitDebit
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
import Model.Transfer as M
import Model.Common as C
import DB.Schema as S
import Http
import Arith

unwrap e Nothing = throw e
unwrap e (Just v) = v

applyFund isDebit ledger fundV = do
  let accountK = unwrap DebitWithoutAccount (fundAccount fundV)
  mAccountV <- get (accountK :: S.Key S.Account)
  let accountV = unwrap (DBLogicError "referenced account not found") mAccountV
  let amount = fundAmount fundV
  let originalBalance = accountBalance accountV
  let newBalance = opAccount originalBalance amount
  when (newBalance < accountMinAllowedBalance accountV) $ throw NoSufficientFunds
  if isDebit
    then do
      let originalLocalHoldBalance = accountHold accountV
      let newLocalHoldBalance = opHold originalLocalHoldBalance amount
      update accountK [AccountBalance =. newBalance,
                       AccountHold =. newLocalHoldBalance]
    else update accountK [AccountBalance =. newBalance]
  -- Hold account (if used)
  when (useHoldAccount ledger) $ do
    mHoldV <- get (holdAccountK ledger)
    let holdV = unwrap (DBLogicError "hold account not found") mHoldV
    let originalHoldBalance = accountBalance holdV
    let newHoldBalance = opHold originalHoldBalance amount
    update (holdAccountK ledger) [AccountBalance =. newHoldBalance]
  -- TODO: if not atomic we should advance to next step immeditely
  return ()
  where (opAccount, opHold) = if isDebit then (sub, add) else (add, sub)

-- | commitDebit
-- Free the funds in escrow (hold)
--
commitDebit ledger fundV = do
  let accountK = unwrap DebitWithoutAccount (fundAccount fundV)
  mAccountV <- get (accountK :: S.Key S.Account)
  let accountV = unwrap (DBLogicError "referenced account not found") mAccountV
  let amount = fundAmount fundV
  let originalLocalHoldBalance = accountHold accountV
  let newLocalHoldBalance = sub originalLocalHoldBalance amount
  update accountK [AccountHold =. newLocalHoldBalance]
  when (useHoldAccount ledger) $ do
    mHoldV <- get (holdAccountK ledger)
    let holdV = unwrap (DBLogicError "hold account not found") mHoldV
    let originalHoldBalance = accountBalance holdV
    let newHoldBalance = sub originalHoldBalance amount
    update (holdAccountK ledger) [AccountBalance =. newHoldBalance]

type ApplyFund =
  forall m. (MonadIO m) => Ledger -> S.Fund -> ReaderT SqlBackend m ()

applyDebit :: ApplyFund
applyDebit = applyFund True

unapplyDebit :: ApplyFund
unapplyDebit = applyFund False

applyCredit :: ApplyFund
applyCredit = applyFund False

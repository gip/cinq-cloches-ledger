{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.Account(
  Account(..),
  hashPassword,
  toEntity,
  fromEntity,
  defaultAccount
  ) where

import GHC.Generics
import Data.Aeson
import Data.Text (Text, pack)
import Data.Text.Encoding
import Control.Monad
import Crypto.Hash

import Models.Common
import Arith
import qualified DB.Schema as S

data Account = Account {
  name :: Text,
  balance :: Maybe Text,
  connector :: Maybe Text,
  password :: Maybe Text,
  publicKey :: Maybe Text,
  isAdmin :: Maybe Bool,
  isDisabled :: Maybe Bool,
  fingerprint :: Maybe Text,
  minimumAllowedBalance :: Maybe Text
} deriving (Show, Generic)
instance FromJSON Account

defaultAccount = Account undefined n n n n n n n n
  where n = Nothing


justOr def maybev = case maybev of Just v -> v
                                   _ -> def

hashPassword :: Text -> Text
hashPassword txt = pack $ show digest
  where digest :: Digest SHA1
        digest = hash (encodeUtf8 txt)

toEntity :: Int -> Account -> S.Account
toEntity scale a =
  S.Account (name a)
            (fromText scale $ justOr "0" (balance a))
            (connector a)
            (liftM hashPassword $ password a)
            (publicKey a)
            (justOr False (Models.Account.isAdmin a))
            (justOr False (isDisabled a))
            (fingerprint a)
            (fromText scale $ justOr "0" (minimumAllowedBalance a))

fromEntity :: Int -> S.Account -> Account
fromEntity scale a =
  Account (S.accountName a)
          (Just . toText scale $ S.accountBalance a)
          (S.accountConnector a)
          (S.accountPasswordHash a)
          (S.accountPublicKey a)
          (Just . S.accountIsAdmin $ a)
          (Just . S.accountIsDisabled $ a)
          (S.accountFingerprint a)
          (Just . toText scale $ S.accountMinAllowedBalance a)

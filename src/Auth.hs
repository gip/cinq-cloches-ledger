{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Auth where

import Network.Wai
import Network.HTTP.Types (status401, hContentType, hAuthorization)
import Database.Persist

import Control.Monad
import qualified Data.Vault.Lazy as V
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (decodeLenient)
import Data.Word8 (isSpace, _colon, toLower)
import Data.Text.Encoding
import Data.Maybe

import Ledger
import Models.Account
import DB.Schema as S

basicAuth ledger app request respond = do
  checkedAuth <- case rawAuth of Just auth -> checkAuth auth
                                 Nothing -> return Nothing
  app (request' checkedAuth) respond
  where
    rawAuth = (lookup hAuthorization $ requestHeaders request)
              >>= extractBasicAuth >>= decode >>= buildAuth
    checkAuth (user, Just hPass) = do
      maybeRow <- runDB ledger $ selectFirst [AccountName ==. user] []
      return $ case maybeRow of
        Just entity ->
          let val = entityVal entity in
          if accountPasswordHash val == Just hPass
            then Just $ Auth user (accountIsAdmin val)
            else Nothing
        Nothing -> Nothing
    checkAuth _ = return Nothing
    buildAuth (user, pass) = Just $ (user, Just $ hashPassword pass)
    request' auth = request { vault = V.insert (keyAuth ledger)
                                               auth
                                               (vault request) }

extractBasicAuth :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
extractBasicAuth bs =
    let (x, y) = BS.break isSpace bs
    in if BS.map toLower x == "basic"
       then extract $ BS.dropWhile isSpace y
       else Nothing
  where
    extract encoded =
        let raw = decodeLenient encoded
            (username, password') = BS.break (== _colon) raw
        in ((username,) . snd) <$> BS.uncons password'

decode (a, b) = Just (decodeUtf8 a, decodeUtf8 b)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Model.Common where

import GHC.Generics
import Data.Aeson
import Data.Text as T

newtype Uuid = Uuid Text
  deriving (Show, Eq, Generic)
instance FromJSON Uuid where
  parseJSON (String s) = if | T.length s == 36 -> return $ Uuid s
                            | otherwise -> fail "could not parse UUID"
  parseJSON _ = fail "could not parse UUID"
instance ToJSON Uuid

uuidToText (Uuid s) = s

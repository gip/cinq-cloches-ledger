{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Fulfillment where

import GHC.Generics
import Data.Aeson
import Data.Text

data FulfillmentResponse = FulfillmentResponse {
  fulfillment :: Text,
  existed :: Bool
} deriving (Show, Generic)
instance ToJSON FulfillmentResponse

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Notification where

import GHC.Generics
import Data.Text
import Data.Aeson

import Model.Transfer

data RelatedResource = RelatedResource {
  execution_condition_fulfillment :: Maybe Text,
  cancellation_condition_fulfillment :: Maybe Text
} deriving (Show, Generic)
instance ToJSON RelatedResource

data Notification = Notification {
  resource :: Transfer,
  related_resource :: Maybe RelatedResource
} deriving (Show, Generic)
instance ToJSON Notification

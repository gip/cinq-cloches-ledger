{-# LANGUAGE OverloadedStrings #-}
module Http where

import Data.Typeable
import qualified Data.ByteString.Lazy as BL
import Control.Exception
import Network.Wai
import Network.HTTP.Types

data ProcessingException =
    NotFound BL.ByteString
  | AlreadExists BL.ByteString
  | WrongFormat BL.ByteString
  | WrongValue BL.ByteString
  | UnprocessableEntity BL.ByteString
  | NotAutorized
  | UnknownMethod
  deriving (Show, Typeable)
instance Exception ProcessingException

caught (NotFound w) =
  responseLBS status404 [] (BL.concat ["not found: ", w])
caught (AlreadExists w) =
  responseLBS status409 [] (BL.concat ["already exists: ", w])
caught (WrongFormat w) =
  responseLBS status404 [] (BL.concat ["wrong format: ", w])
caught (WrongValue w) =
  responseLBS status404 [] (BL.concat ["wrong value: ", w])
caught (UnknownMethod) =
  responseLBS status404 [] "unknown method"
caught (NotAutorized) = responseLBS status401 [] "not authorized"

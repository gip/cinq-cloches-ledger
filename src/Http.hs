{-# LANGUAGE OverloadedStrings #-}
module Http where

import Data.Typeable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Exception
import Network.Wai
import Network.HTTP.Types

standardHeaders :: [(HeaderName, B.ByteString)]
standardHeaders = [(hContentType, "application/json")]

data ProcessingException =
    NotFound BL.ByteString
  | AlreadExists BL.ByteString
  | WrongFormat BL.ByteString
  | WrongValue BL.ByteString
  | UnprocessableEntity BL.ByteString
  | UnrecognizedAccount BL.ByteString
  | NotAutorized
  | UnknownMethod
  | DBLogicError BL.ByteString
  | DebitWithoutAccount
  | CreditWithoutAccount
  | NoSufficientFunds
  | UnmetCondition
  | InvalidFulfillment
  | NoSufficientFundsOnHoldAccount -- should really never happen!
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
caught (UnknownMethod) = responseLBS status404 [] "unknown method"
caught (NotAutorized) = responseLBS status401 [] "not authorized"
caught (UnrecognizedAccount w) =
  responseLBS status401 [] (BL.concat ["unrecognized uri: ", w])
caught (DBLogicError w) = responseLBS status500 [] (BL.concat ["db logic: ", w])
caught (DebitWithoutAccount) =
  responseLBS status500 [] "debit without account"
caught (CreditWithoutAccount) =
  responseLBS status500 [] "credit without account"
caught (NoSufficientFunds) =
  responseLBS status404 [] "no sufficient funds"
caught (UnmetCondition) =
  responseLBS status404 [] "unmet condition"
caught (InvalidFulfillment) =
  responseLBS status404 [] "invalid fulfillment"

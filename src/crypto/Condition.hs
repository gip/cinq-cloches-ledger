{-# LANGUAGE OverloadedStrings #-}
module Crypto.Condition where

import Control.Exception
import Data.ByteString as B
import Data.ByteString.Char8 as B8
import Data.Word8 (_equal)
--import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Crypto.Hash.SHA256 as SHA256

-- See https://interledger.org/five-bells-condition/spec.html
-- This is a (very) partial implementation

data ConditionError =
    ConditionTypeNotSupportedError
  | FulfillmentFormatError
  | FulfillmentPayloadEncodingError
  | ConditionFormatError
  | ConditionFingerprintEncodingError
  | NotImplementedAlgorithmError
  deriving (Show)
instance Exception ConditionError

data ConditionType =
    PreimageSha256  -- 0
  | RsaSha256       -- 1
  | PrefixSha256    -- 2
  | ThresholdSha256 -- 3
  | Ed25519         -- 4
  deriving (Eq, Enum, Show)

bitMask typ =
  case typ of PreimageSha256 -> "3"
              RsaSha256 -> "11"
              PrefixSha256 -> "5"
              ThresholdSha256 -> "9"
              Ed25519 -> "20"

data Condition = Condition {
  conditionType :: !ConditionType,
  featureBitmask :: !ByteString,
  fingerprint :: !ByteString,
  maxFulfillmentLength :: !Int
} deriving (Show)

data Fulfillment = Fulfillment {
  fulfillmentType :: !ConditionType,
  payload :: !ByteString
} deriving (Show)

-- Fulfillment: "cf:" BASE16(type) ":" BASE64URL(payload)
-- Condition: "cc:" BASE16(type) ":" BASE16(featureBitmask) ":" BASE64URL(fingerprint) ":" BASE10(maxFulfillmentLength)

addPaddingB64URL bs =
  case B.length bs `mod` 4 of
    2 -> B.append bs "=="
    3 -> B.append bs "="
    _ -> bs

dropPaddingB64URL bs =
  case B.unsnoc bs of
    Just (bs', c) -> if c == _equal then dropPaddingB64URL bs' else bs
    Nothing -> ""

fulfillmentCondition :: Fulfillment -> Condition
fulfillmentCondition (Fulfillment typ payload) =
  case typ of
    ThresholdSha256 -> Condition typ (bitMask typ) (SHA256.hash payload) maxL
    PreimageSha256 -> Condition typ (bitMask typ) (SHA256.hash payload) maxL
    _ -> throw NotImplementedAlgorithmError
  where sha256Hash = SHA256.hash payload
        maxL = B.length payload

-- TODO: improve this to catch more errors
readHex :: ByteString -> Int
readHex str = read $ '0' : 'x' : (B8.unpack str)

readDec :: ByteString -> Int
readDec str = read $ B8.unpack str

fulfillmentFromURI :: ByteString -> Fulfillment
fulfillmentFromURI uri =
  case B8.split ':' uri of
    ["cf", typeHex, payloadB64URL] ->
      case B64URL.decode $ addPaddingB64URL payloadB64URL of
        Right payload_ -> Fulfillment (toEnum $ readHex typeHex) payload_
        _ -> throw FulfillmentPayloadEncodingError
    _ -> throw FulfillmentFormatError

conditionFromURI :: ByteString -> Condition
conditionFromURI uri =
  case B8.split ':' uri of
    ["cc", typeHex, bm, fingerprintB64URL, maxLength] ->
      case B64URL.decode $ addPaddingB64URL fingerprintB64URL of
        Right fingerprint -> Condition (toEnum $ readHex typeHex)
                                       bm
                                       fingerprint
                                       (readDec maxLength)
        _ -> throw ConditionFingerprintEncodingError
    _ -> throw ConditionFormatError

conditionToURI :: Condition -> ByteString
conditionToURI (Condition typ bm fp ml) =
  B.concat ["cc:", B8.pack $ show $ fromEnum typ,
            ":", bitMask typ,
            ":", dropPaddingB64URL $ B64URL.encode fp,
            ":", B8.pack $ show ml]

verifyFulfillementCondition :: ByteString -> ByteString -> Bool
verifyFulfillementCondition fulfillmentURI conditionURI =
  conditionURI == (conditionToURI $ fulfillmentCondition $ fulfillmentFromURI fulfillmentURI)

test = verifyFulfillementCondition
         "cf:0:7Bcrk61eVjv0kyxw4SRQNMNUZ-8u_U1k6_gZaDRn4r-2IpH62UMvjymLnEpIldvik_b_2hpo2t8Mze9fR6DHISpf6jzal6P0wD6p8uisHOyGpR1FISer26CdG28zHAcK"
         "cc:0:3:8_bQPd36AIxv5pdknF-K577yWrDw3QLZ6C8Iy52VvGE:96"

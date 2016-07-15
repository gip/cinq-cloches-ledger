{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Arith where

import Control.Exception
import Data.Int
import Data.Text as T

type T = Int64

zero :: T
zero = 0

neg :: T -> T
neg x | x == minBound = throw Overflow
      | otherwise = -x

add :: T -> T -> T
add x y =
  if | p x && p y && n sum -> throw Overflow
     | n x && n y && p sum -> throw Underflow
     | otherwise -> sum
  where sum = x + y
        p x = x > 0
        n x = x < 0

sub x y = add x (neg y)

fromText :: Int -> Text -> T
fromText scale txt =
  case splitOn "." txt of
    [i] -> read . unpack $ T.concat [i, padding scale]
    [i, d] ->
      if T.any (/= '0') post then throw LossOfPrecision
                             else read . unpack $
                               T.concat [i, pre, padding $ scale - T.length pre]
        where (pre, post) = T.splitAt scale d
    _ -> error "no parse"
  where padding n = T.replicate n "0"

-- TODO: can we simplify this? Sounds very complex
toText :: Int -> T -> Text
toText 0 x = pack $ show x
toText scale x =
  if | x == 0 -> "0"
     | x > 0 -> toTextPos x
     | otherwise -> T.concat ["-", toTextPos $ neg x]
  where
    toTextPos x =
      case T.splitAt (T.length textPadded - scale) textPadded of
        ("", d) -> T.concat ["0.", d]
        (i, d) -> T.concat [i, ".", d]
      where text = pack $ show x
            textPadded = T.concat [padding $ scale - T.length text, text]
            padding n = T.replicate n "0"

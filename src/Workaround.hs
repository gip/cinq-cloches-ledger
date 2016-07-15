{-# LANGUAGE OverloadedStrings #-}
module Workaround(encodeWithWorkaround) where

import Data.Aeson as A
import Data.Vector as V
import Data.Maybe
import Data.ByteString.Lazy
import Data.HashMap.Strict as HMS

-- Working around Aeson bug #454
--   See https://github.com/bos/aeson/issues/454
--   This should be fixed in Aeson 1.x.y.z but
--   In the meantime the function below is a drop-in replacement for encode
--   Won't show null values in objects

encodeWithWorkaround :: ToJSON a => a -> ByteString
encodeWithWorkaround s =
  A.encode . cleanUp . fromJust . A.decode $ A.encode s

cleanUp (Object m) =
  let m' = HMS.filter (/= Null) m in Object $  HMS.map cleanUp m'
cleanUp (Array a) = Array $ V.map cleanUp a
cleanUp val = val

module Crypto.SHA1 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Crypto.SHA
import Util

keyedSHA :: Key -> ByteString -> Digest SHA1State
keyedSHA k m = sha1 (BL.fromStrict $ k `B.append` m)

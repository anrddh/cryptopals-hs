{- LANGUAGE NoImplicitPrelude #-}

module Util where

import Protolude

import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base64 as B64
import Control.Lens

hexToBase64 :: ByteString -> ByteString
hexToBase64 = B64.encode . fst . Hex.decode

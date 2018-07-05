{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Util where

import Protolude

import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base64 as B64

{- Encoding/Decoding utilities -}

hexToBase64 :: ByteString -> ByteString
hexToBase64 = B64.encode . fst . Hex.decode

base64ToHex :: ByteString -> ByteString
base64ToHex inp = Hex.encode $ case B64.decode inp of
                                 Right s -> s
                                 Left  _ -> ""

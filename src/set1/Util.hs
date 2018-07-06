{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Util where

import Protolude

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base64 as B64

{- Encoding/Decoding utilities -}

hexDecode :: ByteString -> ByteString
hexDecode = fst . Hex.decode

-- Deserializes a string representing Hex and then serializes it to a
-- string representing B64.
hexToBase64 :: ByteString -> ByteString
hexToBase64 = B64.encode . hexDecode

-- Deserializes a string representing B64 and then serializes it to a
-- string representing Hex.
base64ToHex :: ByteString -> ByteString
base64ToHex inp = Hex.encode $ case B64.decode inp of
                                 Right s -> s
                                 Left  _ -> ""

{- XOR -}

-- 'xor' for 'ByteString's
bXor :: ByteString -> ByteString -> ByteString
bXor x y = B.pack $ B.zipWith xor x y

-- 'bXor' but wrapped with a Hex serializer/deserializer
hXor :: ByteString -> ByteString -> ByteString
hXor i1 i2 = Hex.encode $ (hexDecode i1) `bXor` (hexDecode i2)

{-# LANGUAGE OverloadedStrings #-}

module Xor where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T

import Util

-- 'xor' for 'ByteString's
bXor :: ByteString -> ByteString -> ByteString
bXor x y = B.pack $ B.zipWith xor x y

-- 'xor' for 'BL.ByteString's
lXor :: BL.ByteString -> BL.ByteString -> BL.ByteString
lXor x y = BL.pack $ BL.zipWith xor x y

-- 'xor' for 'Decoded'
dXor :: Decoded -> Decoded -> Decoded
dXor d1 d2 = D $ bXor (d d1) (d d2)

-- 'xor' for 'Encodedable a'
eXor :: Encodable a => a -> a -> Either Text a
eXor e1 e2 = do d1 <- decode e1
                d2 <- decode e2
                pure $ encode $ d1 `dXor` d2

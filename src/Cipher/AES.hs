{-# LANGUAGE OverloadedStrings #-}

module Cipher.AES where

import Prelude hiding (succ)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Crypto.Cipher.AES
import Data.Word
import Data.Text.Encoding
import Debug.Trace
import Safe
import Data.Text (Text)
import Text.Megaparsec
import qualified Data.Map as Map
import Data.Word

import Cipher.Rand
import Util
import Xor

data AESMode = ECB | CBC deriving (Eq, Show)

-- PlainText must be padded
eECB :: Key -> PlainText -> CipherText
eECB key inp = encryptECB (initAES key) inp

dECB :: Key -> CipherText -> PlainText
dECB = decryptECB . initAES

-- Only works correctly if len IV = 16 bytes (128 bits) and input is
-- padded
eCBC :: Key -> IV -> PlainText -> CipherText
eCBC _ _ "" = ""
eCBC key iv pt = cipherOut `B.append` restBlocks
  where cipherInp  = B.take 16 pt `bXor` iv
        cipherOut  = eECB key cipherInp
        restBlocks = eCBC key cipherOut (B.drop 16 pt)

-- Only works correctly if len IV = 16 bytes (128 bits)
dCBC :: Key -> IV -> CipherText -> PlainText
dCBC _ _ "" = ""
dCBC key iv ct
  | 16 <= B.length ct = plainOut `B.append` nextBlock
  | otherwise         = error "Ciphertext not block-aligned"
  where cipherInp = B.take 16 ct
        plainOut  = dECB key cipherInp `bXor` iv
        nextBlock = dCBC key cipherInp (B.drop 16 ct)

-- Performs CTR on a block
-- Ignores everything after the 16th byte in the PlainText
ctrBlock :: Key -> Nonce -> Int -> PlainText -> CipherText
ctrBlock k n b p = eECB k ci `bXor` B.take 16 p
  where c  = fromIntegral b                  -- Counter (Word64)
        nB = toLazyByteString $ word64LE n   -- Nonce as a ByteString
        cB = toLazyByteString $ word64LE c   -- Counter as a ByteString
        ci = BL.toStrict $ nB `BL.append` cB -- Cipher input

-- The Key must be 16 bytes; there's a 8 byte limit on both the Nonce
-- and the Counter.
eCTR :: Key -> Nonce -> Counter -> PlainText -> CipherText
eCTR _ _ _ "" = ""
eCTR k n c p  = curr `B.append` rest
  where nB   = toLazyByteString $ word64LE n -- Nonce as a ByteString
        cB   = toLazyByteString $ word64LE c -- Counter as a ByteString
        ci   = BL.toStrict $ nB `BL.append` cB -- Cipher input
        curr = eECB k ci `bXor` (B.take 16 p) -- CipherText of the current block
        rest = eCTR k n (c + 1) (B.drop 16 p) -- Rest of the CipherText

-- Decryption is the same operation as encryption in CTR mdoe
dCTR :: Key -> Nonce -> Counter -> CipherText -> PlainText
dCTR = eCTR
  
isECB :: CipherText -> Bool
isECB bs = hasDuplicates $ breakBtStr bs 16

aesMode :: CipherText -> AESMode
aesMode bs = if isECB bs
             then ECB
             else CBC

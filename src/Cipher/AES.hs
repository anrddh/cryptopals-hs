{-# LANGUAGE OverloadedStrings #-}

module Cipher.AES where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Crypto.Cipher.AES
import Data.Word
import Data.Text.Encoding
import Debug.Trace
import Safe
import Data.Text (Text)
import Text.Megaparsec
import qualified Data.Map as Map

import Cipher.Rand
import Util
import Xor

type Key = ByteString
type PlainText = ByteString
type CipherText = ByteString
type IV = ByteString

data AESMode = ECB | CBC deriving (Eq, Show)

eECB :: Key -> PlainText -> CipherText
eECB key inp = encryptECB (initAES key) inp

-- Key -> CipherText -> PlainText
dECB :: Key -> CipherText -> PlainText
dECB = decryptECB . initAES

-- Only works correctly if len IV = 16 bytes (128 bits) and input is
-- padded
-- Key -> IV -> PlainText -> CipherText
eCBC :: Key -> IV -> PlainText -> CipherText
eCBC _ _ "" = ""
eCBC key iv pt = cipherOut `B.append` restBlocks
  where cipherInp  = B.take 16 pt `bXor` iv
        cipherOut  = eECB key cipherInp
        restBlocks = eCBC key cipherOut (B.drop 16 pt)

-- Only works correctly if len IV = 16 bytes (128 bits)
-- Key -> IV -> CipherText -> PlainText
dCBC :: Key -> IV -> CipherText -> ByteString
dCBC _ _ "" = ""
dCBC key iv ct
  | 16 <= B.length ct = plainOut `B.append` nextBlock
  | otherwise         = error "Ciphertext not block-aligned"
  where cipherInp = B.take 16 ct
        plainOut  = dECB key cipherInp `bXor` iv
        nextBlock = dCBC key cipherInp (B.drop 16 ct)
  
isECB :: CipherText -> Bool
isECB bs = hasDuplicates $ breakBtStr bs 16

aesMode :: CipherText -> AESMode
aesMode bs = if isECB bs
             then ECB
             else CBC

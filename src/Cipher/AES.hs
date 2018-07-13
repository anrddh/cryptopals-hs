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
eECB key inp = encryptECB (initAES key) (pad' inp 16)

-- Key -> CipherText -> PlainText
dECB :: Key -> CipherText -> PlainText
dECB = decryptECB . initAES

-- Only works correctly if len IV = 16 bytes (128 bits)
-- Key -> IV -> PlainText -> CipherText
eCBC :: Key -> IV -> PlainText -> CipherText
eCBC _ _ "" = ""
eCBC key iv pt
  | ivLen <= B.length pt = cipherOut `B.append` nextBlock
  | ivLen > B.length pt  = eCBC key iv $ pad pt ivLen
  where ivLen     = B.length iv
        cipherInp = (B.take ivLen pt) `bXor` iv
        cipherOut = eECB key cipherInp
        nextBlock = eCBC key cipherOut (B.drop ivLen pt)

-- Only works correctly if len IV = 16 bytes (128 bits)
-- Key -> IV -> CipherText -> PlainText
dCBC :: Key -> ByteString -> CipherText -> ByteString
dCBC _ _ "" = ""
dCBC key iv ct
  | ivLen <= B.length ct = plainOut `B.append` nextBlock
  | ivLen > B.length ct  = error "Incorrect padding"
  where ivLen     = B.length iv
        cipherInp = B.take ivLen ct
        plainOut  = (dECB key cipherInp) `bXor` iv
        nextBlock = dCBC key cipherInp (B.drop ivLen ct)
  
isECB :: CipherText -> Bool
isECB bs = hasDuplicates $ breakBtStr bs 16

aesMode :: CipherText -> AESMode
aesMode bs = if isECB bs
             then ECB
             else CBC

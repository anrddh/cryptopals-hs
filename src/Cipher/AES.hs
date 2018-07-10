{-# LANGUAGE OverloadedStrings #-}

module Cipher.AES where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Crypto.Cipher.AES

import Util
import Xor

data AESMode = ECB | CBC deriving (Eq, Show)

-- Key -> PlainText -> CipherText
eECB :: ByteString -> ByteString -> ByteString
eECB = encryptECB . initAES

-- Key -> CipherText -> PlainText
dECB :: ByteString -> ByteString -> ByteString
dECB = decryptECB . initAES

-- Only works correctly if len IV = 16 bytes (128 bits)
-- Key -> IV -> PlainText -> CipherText
eCBC :: ByteString -> ByteString -> ByteString -> ByteString
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
dCBC :: ByteString -> ByteString -> ByteString -> ByteString
dCBC _ _ "" = ""
dCBC key iv ct
  | ivLen <= B.length ct = plainOut `B.append` nextBlock
  | ivLen > B.length ct  = error "Incorrect padding"
  where ivLen     = B.length iv
        cipherInp = B.take ivLen ct
        plainOut  = (dECB key cipherInp) `bXor` iv
        nextBlock = dCBC key cipherInp (B.drop ivLen ct)

-- Length of the input must be a multiple of 16 bytes
isECB :: ByteString -> Bool
isECB = hasDuplicates . flip breakBtStr 16

-- PlainText -> (CipherText, AES EncMode)
randEnc :: ByteString -> (ByteString, AESMode)


-- Length of the input must be a multiple of 16 bytes
oracle :: ByteString -> AESMode
oracle bs = if isECB bs
            then ECB
            else CBC

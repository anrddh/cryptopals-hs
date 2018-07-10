{-# LANGUAGE OverloadedStrings #-}

module Cipher.AES where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Crypto.Cipher.AES
import System.Random
import Control.Monad.State

import Util
import Xor

type RandGen a = State StdGen a

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

-- Generates a random ByteString that's n bytes long given a seed
genBytes :: Int -> Int -> ByteString
genBytes n = B.pack . take n . randoms . mkStdGen

-- Generates a random key given a seed
genKey :: Int -> ByteString
genKey = genBytes 16

-- Given a seed, returns a number between 5 and 10
getCount :: Int -> Int
getCount = fst . randomR (4, 11) . mkStdGen

-- Returns two random ByteStrings of length between 5 and 10 given a
-- seed
genPlaintexts :: Int -> (ByteString, ByteString)
genPlaintexts n = (genBytes count n, genBytes count n)
  where count = getCount n

-- Generates a random bool given a seed
genBool :: Int -> Bool
genBool s = case fst $ randomR (0 :: Int, 3) (mkStdGen s) of
              1 -> True
              2 -> False

-- Seed -> PlainText -> (CipherText, AES EncMode)
randEnc :: Int -> ByteString -> (ByteString, AESMode)
randEnc s pt = case genBool s of
                 True  -> (eECB key plaintext,    ECB)
                 False -> (eCBC key iv plaintext, CBC)
  where key       = genKey s
        (p1, p2)  = genPlaintexts s
        plaintext = p1 `B.append` pt `B.append` p2
        iv        = genBytes 16 s
  
-- Length of the input must be a multiple of 16 bytes
oracle :: ByteString -> AESMode
oracle bs = if isECB bs
            then ECB
            else CBC

{-# LANGUAGE OverloadedStrings #-}

module Cipher.AES where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Crypto.Cipher.AES
import System.Random
import Control.Monad

import Util
import Xor

data AESMode = ECB | CBC deriving (Eq, Show)

-- Key -> PlainText -> CipherText
eECB :: ByteString -> ByteString -> ByteString
eECB key inp = encryptECB (initAES key) (pad' inp 16)

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

-- Generates a random ByteString that's n bytes long
genBytes :: Int -> IO ByteString
genBytes n = replicateM n randomIO
             >>= pure . B.pack

-- Generates a random key
genKey :: IO ByteString
genKey = genBytes 16

-- Returns a number between 5 and 10
getCount :: IO Int
getCount = randomRIO (4, 11)

-- Returns two random ByteStrings of length between 5 and 10
genPlaintexts :: IO (ByteString, ByteString)
genPlaintexts = do x  <- getCount
                   y  <- getCount
                   x' <- genBytes x
                   y' <- genBytes y
                   pure (x', y')

-- Generates a random bool given a seed
genBool :: IO Bool
genBool = do gen <- getStdGen
             let (num, gen') = randomR (0 :: Int, 1) gen
             setStdGen gen'
             pure $ case num of
                      0 -> True
                      1 -> False

-- PlainText -> (CipherText, AES EncMode)
randEnc :: ByteString -> IO (ByteString, AESMode)
randEnc pt = do b <- genBool
                key <- genKey
                (p1, p2) <- genPlaintexts
                iv <- genBytes 16
                let plaintext = p1 `B.append` pt `B.append` p2
                pure $ case b of
                  True  -> (eECB key plaintext,    ECB)
                  False -> (eCBC key iv plaintext, CBC)

isECB :: ByteString -> Bool
isECB bs = hasDuplicates $ breakBtStr bs 16

oracle :: ByteString -> AESMode
oracle bs = if isECB bs
            then ECB
            else CBC

{-# LANGUAGE OverloadedStrings #-}

module Cipher.AES where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Crypto.Cipher.AES
import System.Random
import Control.Monad
import Data.Word
import Data.Text.Encoding
import Debug.Trace
import Safe

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
oracle :: PlainText -> IO (CipherText, AESMode)
oracle pt = do b <- genBool
               key <- genKey
               (p1, p2) <- genPlaintexts
               iv <- genBytes 16
               let plaintext = p1 `B.append` pt `B.append` p2
               pure $ case b of
                 True  -> (eECB key plaintext,    ECB)
                 False -> (eCBC key iv plaintext, CBC)

-- This is for the purposes of Challenge 12
-- PlainText -> CipherText
oracle' :: PlainText -> CipherText
oracle' pt = eECB "YELLOW SUBMARINE" (pt `B.append` (d $ right $ decode $ B64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"))
  where right (Right x) = x

aesBlockSize :: Int
aesBlockSize = 16

getBlocks :: Int -> ByteString -> ByteString
getBlocks =  B.take . (* aesBlockSize)

-- Curr -> Inp -> Byte
bruteForceByte :: Int -> ByteString -> ByteString -> Maybe Word8
bruteForceByte b c i =
  headMay $
  filter
  ((getBlocks b (oracle' i) ==) . getBlocks b . oracle' . B.snoc (i `B.append` c))
  [0..255]

-- Current -> Rest
break'C12 :: ByteString -> ByteString
break'C12 curr = case (bruteForceByte currBlock curr inp) of
                   Just b  -> break'C12 $ curr `B.snoc` b
                   Nothing -> curr
  where outLen    = B.length $ oracle' ""
        currLen   = B.length curr
        remLen    = outLen - currLen
        inpLen    = currBlock * aesBlockSize - currLen - 1
        inp       = charRepl inpLen 65 -- 65 == 'A' (this is an arbitrary choice)
        currBlock = currLen `div` aesBlockSize + 1
  
isECB :: CipherText -> Bool
isECB bs = hasDuplicates $ breakBtStr bs 16

aesMode :: CipherText -> AESMode
aesMode bs = if isECB bs
             then ECB
             else CBC

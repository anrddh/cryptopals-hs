{-# LANGUAGE OverloadedStrings #-}

module S3.C17 where

import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Lens
import System.Random
import Data.Word

import Crypto.AES
import Crypto.Rand
import Util

plaintexts :: [ByteString]
plaintexts = d . right . decode . B64 <$>
  ["MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=", 
   "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=",
   "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==",
   "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==",
   "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl",
   "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==",
   "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==",
   "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=",
   "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=",
   "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"]
  where right (Right x) = x

genEncryptor :: IO (PlainText -> (CipherText, IV), CipherText -> PlainText)
genEncryptor = do key <- genKey
                  iv <- genBytes 16
                  pure (flip (,) iv . eCBC key iv, dCBC key iv)

getOracle :: (CipherText -> PlainText) -> CipherText -> Bool
getOracle dec ct = case stripPad $ dec ct of
                     Just _  -> True
                     Nothing -> False

break :: (CipherText -> PlainText) -> CipherText -> PlainText
break dec = break' oracle 0
  where oracle = getOracle dec

consCipherText :: (CipherText -> Bool) -> CipherText -> Int -> CipherText
consCipherText o c pos = undefined -- o . (setAt c pos) <$> [0..255]

break' :: (CipherText -> Bool) -> Int -> CipherText -> PlainText
break' oracle pos c = curr `B.cons` rest
  where curr = undefined
        rest = break' oracle (pos + 1) c

runS3C17 :: IO ()
runS3C17 = do (enc, oracle) <- genEncryptor
              undefined

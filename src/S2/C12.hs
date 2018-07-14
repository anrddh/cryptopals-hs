{-# LANGUAGE OverloadedStrings #-}

module S2.C12 where

import Prelude

import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Text.Read
import Data.Text.Encoding
import System.IO (hFlush, stdout)
import Data.Word
import Safe

import Cipher.AES
import Util

-- PlainText -> CipherText
oracle :: PlainText -> CipherText
oracle pt = eECB "YELLOW SUBMARINE" (pt `B.append` (d $ right $ decode $ B64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"))
  where right (Right x) = x

getBlks :: Int -> ByteString -> ByteString
getBlks =  B.take . (* 16)

-- Curr -> Inp -> Byte
bruteForceByte :: Int -> ByteString -> ByteString -> Maybe Word8
bruteForceByte b c i =
  headMay $
    filter
      ((getBlks b (oracle i) ==) . getBlks b . oracle . B.snoc (i `B.append` c))
      [0..255]

-- Current -> Rest
break'C12 :: ByteString -> ByteString
break'C12 curr = case (bruteForceByte currBlock curr inp) of
                   Just b  -> break'C12 $ curr `B.snoc` b
                   Nothing -> curr
  where currLen   = B.length curr
        inpLen    = currBlock * 16 - currLen - 1
        inp       = charRepl inpLen 65 -- 65 ~ 'A' (this is an arbitrary choice)
        currBlock = currLen `div` 16 + 1

runS2C12 :: IO ()
runS2C12 = do putStrLn . show $ break'C12 ""

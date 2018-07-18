{-# LANGUAGE OverloadedStrings #-}

module S2.C11 where

import Prelude hiding (putStr, putStrLn)

import Data.Bifunctor
import qualified Data.ByteString as B
import Data.Text.IO hiding (getLine)
import Text.Read
import Data.Text.Encoding
import System.IO (hFlush, stdout)

import Crypto.AES
import Crypto.Rand
import Util

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

runS2C11 :: IO ()
runS2C11 = do putStr "Input: "
              hFlush stdout
              inp <- B.getLine
              first aesMode <$> oracle inp >>= print

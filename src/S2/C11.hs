{-# LANGUAGE OverloadedStrings #-}

module S2.C11 where

import Prelude hiding (putStr, putStrLn)

import Data.Bifunctor
import qualified Data.ByteString as B
import Data.Text.IO hiding (getLine)
import Text.Read
import Data.Text.Encoding
import System.IO (hFlush, stdout)

import Cipher.AES
import Util

runS2C11 :: IO ()
runS2C11 = do putStr "Input: "
              hFlush stdout
              inp <- B.getLine
              first aesMode <$> oracle inp >>= print

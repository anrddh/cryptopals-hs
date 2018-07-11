{-# LANGUAGE OverloadedStrings #-}

module S2.C12 where

import Prelude

import Data.Bifunctor
import qualified Data.ByteString as B
import Text.Read
import Data.Text.Encoding
import System.IO (hFlush, stdout)

import Cipher.AES
import Util

runS2C12 :: IO ()
runS2C12 = do putStrLn . show $ break'C12 ""

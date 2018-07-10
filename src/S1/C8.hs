{-# LANGUAGE OverloadedStrings #-}

module S1.C8 where

import Prelude hiding (putStr, putStrLn)

import qualified Data.ByteString as B
import Control.Monad
import Data.Text
import Data.Text.IO
import Data.Text.Encoding

import Util
import Cipher

runS1C8 :: IO ()
runS1C8 = do file <- readHexFile "c8.txt"
             print $ decode <$> file

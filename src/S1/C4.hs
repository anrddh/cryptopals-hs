{-# LANGUAGE OverloadedStrings #-}

module S1.C4 where

import Prelude hiding (putStr, putStrLn)

import System.IO (hFlush, stdout)
import qualified Data.ByteString as B
import Data.Text
import Data.Text.IO

import Util
import Crypto.SingleXor

runS1C4 :: IO ()
runS1C4 = do hexs <- readHexFile "c4.txt"
             case detectSingleXor hexs of
               Left  s -> putStrLn $ "Error: " `append` s
               Right s -> putStrLn $ fst s

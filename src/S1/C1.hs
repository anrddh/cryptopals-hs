{-# LANGUAGE OverloadedStrings #-}

module S1.C1 where

import Prelude hiding (putStr, putStrLn)

import qualified Data.ByteString as B
import System.IO (hFlush, stdout)
import Data.Text
import Data.Text.IO

import Util

runS1C1 :: IO ()
runS1C1 = putStr "Enter hex string: "
        *>  hFlush stdout
        *>  B.getLine
        >>= \line -> case hexToBase64 $ B16 line of
                       Right s -> putStrLn $ pretty s
                       Left s  -> putStrLn $ "Error: " `append` s

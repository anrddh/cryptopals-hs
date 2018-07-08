{-# LANGUAGE OverloadedStrings #-}

module C2 where

import Prelude hiding (putStr, putStrLn)

import qualified Data.ByteString as B
import System.IO (hFlush, stdout)
import Data.Text
import Data.Text.IO

import Util
import Xor

runS1C2 :: IO ()
runS1C2 = do putStr "Enter hex string 1: "
             hFlush stdout
             line1 <- B16 <$> B.getLine
             putStr "Enter hex string 2: "
             hFlush stdout
             line2 <- B16 <$> B.getLine
             case line1 `eXor` line2 of
               Right s -> putStrLn $ pretty s
               Left s  -> putStrLn $ "Error: " `append` s

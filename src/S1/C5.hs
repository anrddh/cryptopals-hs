{-# LANGUAGE OverloadedStrings #-}

module S1.C5 where

import Prelude hiding (putStr, putStrLn)

import System.IO (hFlush, stdout)
import qualified Data.ByteString as B
import Data.Text
import Data.Text.IO
import Data.Text.Encoding

import Util
import Cipher

runS1C5 :: IO ()
runS1C5 = do file <- D <$> (B.readFile "c5.txt")
             putStr "Enter key: "
             hFlush stdout
             key <- D <$> B.getLine
             putStrLn $ pretty (encode $ repeatkey key file :: Base16)

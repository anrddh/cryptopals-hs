{-# LANGUAGE OverloadedStrings #-}

module S1.C3 where

import Prelude hiding (putStr, putStrLn)

import System.IO (hFlush, stdout)
import qualified Data.ByteString as B
import Data.Text
import Data.Text.IO

import Util
import Crypto.SingleXor

runS1C3 :: IO ()
runS1C3 = do putStr "Enter hex string: "
             hFlush stdout
             ciphertext <- B16 <$> B.getLine
             case singleXorE ciphertext of
               Right s -> putStrLn $ fst s
               Left  s -> putStrLn $ "Error: " `append` s

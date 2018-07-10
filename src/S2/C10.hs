{-# LANGUAGE OverloadedStrings #-}

module S2.C10 where

import Prelude hiding (putStr, putStrLn)

import qualified Data.ByteString as B
import Data.Text.IO
import Data.Text.Encoding
import System.IO (hFlush, stdout)

import Cipher.AES
import Util

runS2C10 :: IO ()
runS2C10 = do cipherText <- B64 <$> B.readFile "c10.txt"
              putStr "Enter key: "
              hFlush stdout
              key <- B.getLine
              case decode cipherText of
                Left _       -> putStrLn "Error"
                Right (D ct) -> putStrLn $ decodeUtf8 $ dCBC key (pad (B.pack [0]) (B.length key)) (ct)

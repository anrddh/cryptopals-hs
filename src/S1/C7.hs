{-# LANGUAGE OverloadedStrings #-}

module S1.C7 where

import Prelude hiding (putStr, putStrLn)

import Crypto.Cipher.AES
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text
import Data.Text.IO
import Data.Text.Encoding

import Util
import Crypto.AES

runS1C7 :: IO ()
runS1C7 = do file <- B64 <$> (B.readFile "c7.txt")
             let key = initAES ("YELLOW SUBMARINE" :: ByteString)
             case decode file of
               Right (D s) -> putStrLn $ decodeUtf8 $ decryptECB key s
               Left  s     -> putStrLn "Error"

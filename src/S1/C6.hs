{-# LANGUAGE OverloadedStrings #-}

module S1.C6 where

import Prelude hiding (putStr, putStrLn)

import qualified Data.ByteString as B
import Data.Text
import Data.Text.IO
import Data.Text.Encoding

import Util
import Cipher.RepeatKey

runS1C6 :: IO ()
runS1C6 = do file <- B64 <$> (B.readFile "c6.txt")
             case decode file of
               Right s -> do
                 let (plaintext, key) = breakRepeatKey s
                 putStrLn $ "Key: " `append` "«" `append` key `append` "»"
                 putStrLn $ "Plaintext:\n" `append` plaintext
               Left  s -> putStrLn "Error"

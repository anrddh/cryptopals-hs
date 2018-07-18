{-# LANGUAGE OverloadedStrings #-}

module S3.C18 where

import Data.ByteString (ByteString)
import Data.Text.Encoding

import Crypto.AES
import Util

ct :: ByteString
ct = d $ right $ decode $ B64
     "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
  where right (Right x) = x

runS3C18 :: IO ()
runS3C18 = do
  putStrLn $ show $ decodeUtf8 $ eCTR "YELLOW SUBMARINE" 0 0 ct

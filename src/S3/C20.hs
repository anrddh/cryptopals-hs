{-# LANGUAGE OverloadedStrings #-}

module S3.C20 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Foldable
import qualified Data.Text.IO as TIO

import Crypto.AES
import Crypto.RepeatKey
import Util

minLength :: (Foldable a, Functor a) => a ByteString -> Int
minLength ls = minimum $ B.length <$> ls

runS3C20 :: IO ()
runS3C20 = do
  inp <- (d . right . decode . B64 . encodeUtf8 <$>) . T.lines <$> TIO.readFile "20.txt"
  let cts       = eCTR "YELLOW SUBMARINE" 0 0 <$> inp
      ctsMinLen = minLength cts
      ct        = B.concat $ B.take ctsMinLen <$> cts
  TIO.putStrLn $ fst $ breakRepeatKey' ctsMinLen $ D ct
  where right (Right x) = x

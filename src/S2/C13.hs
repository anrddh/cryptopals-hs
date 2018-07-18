{-# LANGUAGE OverloadedStrings #-}

module S2.C13 where

import Prelude

import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Text.Read
import Data.Text (Text)
import Data.Text.Encoding
import System.IO (hFlush, stdout)
import Data.Word
import Safe

import Crypto.AES
import Util

c13Key :: ByteString
c13Key = "YELLOW SUBMARINE"

oracle :: Text -> CipherText
oracle = eECB c13Key . encodeUtf8 . encodeQuery . profileFor

decryptor :: CipherText -> Text
decryptor = decodeUtf8 . dECB c13Key

-- decryptor consC13 = "email=AAAAAAAAAAAAA&uid=10&role=admin\v\v\v\v\v\v\v\v\v\v\v"
consC13 :: CipherText
consC13 = prefix `B.append` postfix
  where email1  = "AAAAAAAAAAAAA" -- email len must be 13
        email2  = "AAAAAAAAAA"
        prefix  = B.take 32 $ oracle email1
        postfix = B.take 16 $ B.drop 16 $ oracle $ decodeUtf8 $ email2 `B.append` (pad' "admin" 16) -- The As in this construction are completely garbage

runS2C13 :: IO ()
runS2C13 = putStrLn $ show $ decryptor consC13

{-# LANGUAGE OverloadedStrings #-}

module S4.C26 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Crypto.SHA1

-- "random"
randomKey :: ByteString
randomKey = "YELLOW SUBMARINE"

message :: ByteString
message = "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"

message' :: ByteString
message' = ";admin=true"

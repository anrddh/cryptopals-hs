{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}

module Util where

import Protolude

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base64 as B64

newtype Base16  = B16  { b16 :: ByteString } deriving (Eq, Show)
newtype Base64  = B64  { b64 :: ByteString } deriving (Eq, Show)
newtype Decoded = D { d :: ByteString } deriving (Eq, Show)

-- encode . decode = id = decode . encode
class Encodable a where
  decode :: a -> Either Text Decoded
  encode :: Decoded -> a

{- Encoding/Decoding utilities -}

instance Encodable Base16 where
  decode (B16 bs) = case Hex.decode bs of
                      (e, "") -> Right $ D e
                      _       -> Left "Decode error"
  encode = B16 . Hex.encode . d

instance Encodable Base64 where
  decode bs = case B64.decode $ b64 bs of
                Right s -> Right $ D s
                Left e  -> Left $ show e
  encode = B64 . B64.encode . d

-- Deserializes a string representing Hex and then serializes it to a
-- string representing B64.
hexToBase64 :: Base16 -> Either Text Base64
hexToBase64 bs = decode bs >>= pure . encode

-- Deserializes a string representing B64 and then serializes it to a
-- string representing Hex.
base64ToHex :: Base64 -> Either Text Base16
base64ToHex bs = decode bs >>= pure . encode

{- Pretty -}

-- Alternative to 'Show' for pretty-printing purposes
class Pretty a where
  pretty :: a -> Text

instance Pretty (Either UnicodeException Text) where
  pretty (Right s) = s
  pretty (Left e)  = "Error: " <> show e
 
instance Pretty Base16 where
  pretty = toS . b16

instance Pretty Base64 where
  pretty = toS . b64

{- XOR -}

-- 'xor' for 'ByteString's
bXor :: ByteString -> ByteString -> ByteString
bXor x y = B.pack $ B.zipWith xor x y

-- 'xor' for 'Decoded'
dXor :: Decoded -> Decoded -> Decoded
dXor d1 d2 = D $ bXor (d d1) (d d2)

-- 'xor' for 'Encodedable a'
eXor :: Encodable a => a -> a -> Either Text a
eXor e1 e2 = do d1 <- decode e1
                d2 <- decode e2
                pure $ encode $ d1 `dXor` d2

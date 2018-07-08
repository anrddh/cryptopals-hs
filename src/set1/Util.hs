{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Util where

import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base64 as B64
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import TextShow

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
                Left e  -> Left $ showt e
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

instance Pretty Base16 where
  pretty = showt . b16

instance Pretty Base64 where
  pretty = showt . b64

readHexFile :: Text -> IO [Base16]
readHexFile f = ((B16 . encodeUtf8 <$>) . T.lines) <$> TIO.readFile (T.unpack f)

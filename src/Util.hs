{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Util where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import TextShow
import Data.Maybe
import Control.Lens
import Data.List
import Data.Word
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Applicative hiding (some)
import Control.Monad

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
  decode (B64 bs) = case B64.decode bs of
                      Right s -> Right $ D s
                      Left  e -> Left $ showt e
  encode = B64 . B64.encode . d

reencode :: (Encodable a, Encodable b) => a -> Either Text b
reencode bs = decode bs >>= pure . encode

-- Deserializes a string representing Hex and then serializes it to a
-- string representing B64.
hexToBase64 :: Base16 -> Either Text Base64
hexToBase64 = reencode

-- Deserializes a string representing B64 and then serializes it to a
-- string representing Hex.
base64ToHex :: Base64 -> Either Text Base16
base64ToHex = reencode

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

-- Input -> Pad Size -> Padded
pad :: ByteString -> Int -> ByteString
pad d' n
  | B.length d' >= n = d'
  | otherwise        = d' `B.append` pad
  where diff = n - B.length d'
        pad  = B.pack $ replicate diff $ fromIntegral diff

-- Input -> MultipleOf -> Padded
pad' :: ByteString -> Int -> ByteString
pad' bs m = if bsLen `mod` m == 0
            then pad bs $ bsLen + m
            else pad bs $ bsLen + (m - bsLen `mod` m)
  where bsLen = B.length bs

-- Strips (valid) PKCS#7 padding
stripPad :: ByteString -> Maybe ByteString
stripPad "" = Nothing
stripPad b  = case (charRepl (fromIntegral lB) lB) `B.isSuffixOf` b of
                True  -> Just $ B.take (B.length b - (fromIntegral lB)) $ b
                False -> Nothing
  where lB = B.last b

breakBtStr :: ByteString -> Int -> [ByteString]
breakBtStr "" _ = []
breakBtStr b n  = B.take n b : (breakBtStr (B.drop n b) n)

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates ts = length ts /= length (nub ts)

charRepl :: Int -> Word8 -> ByteString
charRepl s = B.pack . replicate s

dRepl :: Int -> Word8 -> Decoded
dRepl x y = D $ charRepl x y

{- Parsing Stuff -}

type Parser = Parsec Void Text

query1 :: Parser (Text, Text)
query1 = liftA2 (,) (T.pack <$> (some $ notChar '='))
                    (T.pack <$> (some $ notChar '&'))

query :: Parser [(Text, Text)]
query = query1 `sepBy` (L.symbol (pure ()) "&")

encodeQuery :: [(Text, Text)] -> Text
encodeQuery = foldr (\(key,val) acc -> T.concat [key, "=", val, "&", acc]) ""

stripBadChars :: Text -> Text
stripBadChars = T.filter $ liftA2 (&&) ('=' /=) ('&' /=)

profileFor :: Text -> [(Text, Text)]
profileFor em = [("email", stripBadChars em),
                 ("uid", "10"),
                 ("role", "user")]

prefixLen :: ByteString -> ByteString -> Int
prefixLen = prefixLen' 0

prefixLen' :: Int -> ByteString -> ByteString -> Int
prefixLen' n "" _  = n
prefixLen' n _ ""  = n
prefixLen' n xs ys = if B.head xs == B.head ys
                     then prefixLen' (n + 1) (B.tail xs) (B.tail ys)
                     else n

-- Round-up division
div' :: (Integral a, Integral b, Integral c) => a -> b -> c
div' x y = if x `mod` y' == 0
           then fromIntegral $ x `div` y'
           else fromIntegral $ x `div` y' + 1
  where y' = fromIntegral y

setAt :: ByteString -> Int -> Word8 -> ByteString
setAt b pos w = b & ix pos .~ w

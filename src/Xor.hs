{-# LANGUAGE OverloadedStrings #-}

module Xor where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Either
import Data.Foldable
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import Data.Word

import Util

-- 'xor' for 'ByteString's
bXor :: ByteString -> ByteString -> ByteString
bXor x y = B.pack $ B.zipWith xor x y

-- 'xor' for 'BL.ByteString's
lXor :: BL.ByteString -> BL.ByteString -> BL.ByteString
lXor x y = BL.pack $ BL.zipWith xor x y

-- 'xor' for 'Decoded'
dXor :: Decoded -> Decoded -> Decoded
dXor d1 d2 = D $ bXor (d d1) (d d2)

-- 'xor' for 'Encodedable a'
eXor :: Encodable a => a -> a -> Either Text a
eXor e1 e2 = do d1 <- decode e1
                d2 <- decode e2
                pure $ encode $ d1 `dXor` d2

charRepl :: Int -> Word8 -> ByteString
charRepl s = B.pack . replicate s

dRepl :: Int -> Word8 -> Decoded
dRepl x y = D $ charRepl x y

scoreStr :: [Char] -> ([Char], Int)
scoreStr []         = ([], 0)
scoreStr cs'@(c:cs) = if isAlpha c then
                        (cs', snd csScore + 2)
                      else
                        if isAlphaNum c || isSpace c then
                          (cs', snd csScore + 1)
                        else
                          (cs', snd csScore)
  where csScore = scoreStr cs

maxScore :: [([Char], Int)] -> Text
maxScore = T.pack . fst . maximumBy (comparing snd)

getLists :: Decoded -> [Decoded]
getLists ct = ((dRepl $ B.length $ d ct) <$> [0..255])

getBest :: [Text] -> Maybe Text
getBest [] = Nothing
getBest xs = Just $ maxScore $ ((scoreStr <$> T.unpack) <$>) xs

singleXorD :: Decoded -> Maybe Text
singleXorD ct = getBest $ rights $ decodeUtf8' <$> d <$> dXor ct <$> getLists ct

singleXorE :: Encodable a => a -> Either Text Text
singleXorE e = decode e >>= \e' -> case singleXorD e' of
                                     Just e'' -> Right e''
                                     Nothing  -> Left "sxe: Error: None of the hexes contained valid UTF8 data"

detectSingleXor :: Encodable a => [a] -> Either Text Text
detectSingleXor es = case getBest $ rights $ singleXorE <$> es of
                       Just e' -> pure e'
                       Nothing -> Left "dsx: Error: None of the hexes contained valid UTF8 data"

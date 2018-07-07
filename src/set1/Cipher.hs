{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Cipher where

import Protolude

import qualified Data.ByteString as B
import Data.Char
import Data.Either
import Data.Foldable

import Util

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
maxScore = toS . fst . maximumBy (comparing snd)

getLists :: Decoded -> [Decoded]
getLists ct = ((dRepl $ B.length $ d ct) <$> [0..255])

getBest :: [Text] -> Text
getBest = maxScore . ((scoreStr <$> toS) <$>)

singleXorD :: Decoded -> Text
singleXorD ct = getBest $ rights $ decodeUtf8' <$> d <$> dXor ct <$> getLists ct

singleXorE :: Encodable a => a -> Either Text Text
singleXorE e = decode e >>= pure . singleXorD


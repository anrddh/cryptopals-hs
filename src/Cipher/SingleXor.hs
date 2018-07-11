{-# LANGUAGE OverloadedStrings #-}

module Cipher.SingleXor
  (singleXorD,
   singleXorE,
   detectSingleXor)
where

import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char
import Data.Either
import Data.Foldable
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word

import Util
import Xor

-- Computes the "score" of the input string and returns the string
-- along with the score.
scoreStr :: String -> (Text, Int)
scoreStr []         = ("", 0)
scoreStr cs'@(c:cs) = if isAlpha c then
                        (T.pack cs', snd csScore + 2)
                      else
                        if isAlphaNum c || isSpace c then
                          (T.pack cs', snd csScore + 1)
                        else
                          (T.pack cs', snd csScore)
  where csScore = scoreStr cs

-- Given a list of strings and their scores, return the string with
-- the maximum score
maxScore :: [((Text, Int), a)] -> Maybe (Text, a)
maxScore [] = Nothing
maxScore xs = Just $ filterSnd $ maximumBy (comparing snd') (merge <$> xs)
  where merge :: ((a,b),c) -> (a,b,c)
        merge ((x,y),z) = (x,y,z)

        snd' :: (a,b,c) -> b
        snd' (_,x,_) = x

        filterSnd :: (a,b,c) -> (a,c)
        filterSnd (x,_,y) = (x,y)

-- Given a Decoded ByteString of length n, return a list of 
getLists :: Decoded -> [(Decoded, Word8)]
getLists ct = (\i -> (dRepl (B.length $ d ct) i, i)) <$> [0..255]

-- Given pairs of 'PlainText's and 'Key's, return the highest scoring
-- PlainText
getBest :: [(Text, Word8)] -> Maybe (Text, Word8)
getBest [] = Nothing
getBest xs = maxScore $ (first (scoreStr . T.unpack)) <$> xs

rights' :: [(Either a b, c)] -> [(b,c)]
rights' []                = []
rights' ((Left _, _):xs)  = rights' xs
rights' ((Right x, y):xs) = (x,y) : rights' xs

-- CipherText -> Maybe (PlainText, Key)
singleXorD :: Decoded -> Maybe (Text, Word8)
singleXorD ct = getBest . rights' $ (first $ decodeUtf8' . d . dXor ct) <$> getLists ct

singleXorE :: Encodable a => a -> Either Text (Text, Word8)
singleXorE e = decode e >>= \e' -> case singleXorD e' of
                                     Just e'' -> Right e''
                                     Nothing  -> Left "sxe: Error: None of the hexes contained valid UTF8 data"

detectSingleXor :: Encodable a => [a] -> Either Text (Text, Word8)
detectSingleXor es = case getBest . rights $ singleXorE <$> es of
                       Just e' -> pure e'
                       Nothing -> Left "dsx: Error: None of the hexes contained valid UTF8 data"

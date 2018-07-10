{-# LANGUAGE OverloadedStrings #-}

module Cipher.RepeatKey
  (repeatkey,
   repeatKeyEnc,
   breakRepeatKey)
where

import Data.Either
import Data.Maybe
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text(Text)
import Data.Text.Encoding
import Data.List (transpose)
import Debug.Trace (trace)
import Data.Ord
import Data.Foldable
import Debug.Trace(trace)

import Cipher.SingleXor
import Util
import Xor

-- Key -> PlainText -> CipherText
repeatkey :: Decoded -> Decoded -> Decoded
repeatkey (D k) (D pt) = D $ BL.toStrict $
                         BL.cycle (BL.fromStrict k) `lXor` (BL.fromStrict pt)

-- repeat key for encoded strings
repeatKeyEnc :: (Encodable a, Encodable b, Encodable c)
             => a
             -> b
             -> Either Text c
repeatKeyEnc keyE ptE = decode keyE
                        >>= \key -> decode ptE
                                    >>= pure . encode . repeatkey key

editDist :: Decoded -> Decoded -> Int
editDist (D h1) (D h2) = sum $ popCount <$> (B.unpack $ h1 `bXor` h2)

breakBtStr :: ByteString -> Int -> [ByteString]
breakBtStr "" _ = []
breakBtStr b n  = B.take n b : (breakBtStr (B.drop n b) n)

-- Inp -> BlockSize -> BlockNum -> Out
getBlock :: Decoded -> Int -> Int -> Decoded
getBlock (D d') n 0 = D $ B.take n d'
getBlock (D d') n b = getBlock (D $ B.drop n d') n (b - 1)

av :: (Integral a, Fractional b, Show a) => [a] -> b
av x = (fromIntegral $ sum x) / (fromIntegral $ length x)

avDists :: Fractional b => [Decoded] -> b
avDists d' = av $ uncurry editDist <$> [(x,y) | x <- d', y <- d']

breakRepeatKey :: Decoded -> (Text, Text)
breakRepeatKey d''@(D d') = (decodeUtf8 $ d $ repeatkey key d'', decodeUtf8 $ d key)
  where f i     = ((avDists [getBlock d'' i 0,
                             getBlock d'' i 1,
                             getBlock d'' i 2,
                             getBlock d'' i 3]) / (fromIntegral i), i)
        keysize = snd $ minimumBy (comparing fst) $ f <$> [2..40]
        key     = D $ B.pack $ fromIntegral . snd <$> blocks
        blocks  = (fromJust . singleXorD . D . B.pack <$> (transpose $ B.unpack <$> (breakBtStr d' keysize)))

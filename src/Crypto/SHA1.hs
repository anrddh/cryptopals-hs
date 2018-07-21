{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Crypto.SHA1 where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import qualified Data.Serialize as S
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import Data.Word

import Debug.Trace

import Util

{- Begin SHA1 Implementation -}

data SHA1State = S Word32 Word32 Word32 Word32 Word32
  deriving Show

initialState :: SHA1State
initialState = S 0x67452301 0xEFCDAB89 0x98BADCFE 0x10325476 0xC3D2E1F0

-- Computes the padding of a `count`-bit message
padding :: Int -> ByteString
padding count = 0x80 `B.cons` (B.replicate padLen 0) `B.append` lenBS
  where padLen   = (119 - count) `mod` 64

        toWord64 :: Integral a => a -> Word64
        toWord64 = fromIntegral

        lenBS    = BL.toStrict . toLazyByteString . word64BE $ toWord64 (8 * count)

padLen :: Int -> Int
padLen = B.length . padding

word32ToBS :: Word32 -> ByteString
word32ToBS = BL.toStrict . toLazyByteString . word32BE

bsToIntegral :: (Integral a, Bits a) => ByteString -> a
bsToIntegral "" = 0
bsToIntegral xs = (bsToIntegral (B.init xs) `shiftL` 8) + (fromIntegral $ B.last xs)

sha1Pad :: ByteString -> ByteString
sha1Pad m = m `B.append` (padding (B.length m))

bsToWords :: Int -> Vector Word32 -> ByteString -> Vector Word32
bsToWords i dest "" = dest
bsToWords i dest bs = bsToWords (i + 1) curr rest
  where curr = V.update dest [(i, bsToIntegral (B.take 4 bs))]
        rest = B.drop 4 bs

extend :: Int -> Vector Word32 -> Vector Word32
extend 80 ws = ws
extend i  ws = extend (i + 1) $ V.update ws [(i, dat)]
  where dat = (ws ! (i - 3)  `xor`
               ws ! (i - 8)  `xor`
               ws ! (i - 14) `xor`
               ws ! (i - 16)) `rotateL` 1

mainLoop :: Int -> SHA1State -> Vector Word32 -> SHA1State
mainLoop i s@(S a b c d e) w
  | 0 <= i && i <= 19  = rest ((b .&. c) .|. ((complement b) .&. d),0x5A827999)
  | 20 <= i && i <= 39 = rest (b `xor` c `xor` d,0x6ED9EBA1)
  | 40 <= i && i <= 59 = rest ((b .&. c) .|. (b .&. d) .|. (c .&. d),0x8F1BBCDC)
  | 60 <= i && i <= 79 = rest (b `xor` c `xor` d,0xCA62C1D6)
  | otherwise          = s
  where rest :: (Word32, Word32) -> SHA1State
        rest (f,k) = mainLoop (i + 1) (S a' b' c' d' e') w
          where temp = (a `rotateL` 5) + f + e + k + w ! i
                e' = d
                d' = c
                c' = b `rotateL` 30
                b' = a
                a' = temp

processChunk :: SHA1State -> ByteString -> SHA1State
processChunk s@(S a b c d e) bs = S (a + a') (b + b') (c + c') (d + d') (e + e')
  where words            = bsToWords 0 (V.replicate 80 0) bs
        ext              = extend 16 words
        S a' b' c' d' e' = mainLoop 0 s ext

toChunks :: ByteString -> [ByteString]
toChunks "" = []
toChunks bs = B.take 64 bs : toChunks (B.drop 64 bs)
  
-- Input must be padded
sha1' :: ByteString -> SHA1State -> SHA1State
sha1' bs s = foldl processChunk s chunks
  where chunks = toChunks bs

-- Input must be padded
sha1'' :: ByteString -> SHA1State
sha1'' = flip sha1' initialState

integralToBs :: (Integral a, Bits a) => a -> ByteString
integralToBs 0 = ""
integralToBs i = integralToBs (i `shiftR` 8) `B.snoc` (fromIntegral $ i .&. 0xFF)

stateToHash :: SHA1State -> ByteString
stateToHash (S a b c d e) = integralToBs num
  where a'  = (fromIntegral a :: Integer) `shiftL` 128
        b'  = (fromIntegral b :: Integer) `shiftL` 96
        c'  = (fromIntegral c :: Integer) `shiftL` 64
        d'  = (fromIntegral d :: Integer) `shiftL` 32
        e'  = (fromIntegral e :: Integer)
        num = a' + b' + c' + d' + e'

-- Input must be padded
sha1 :: ByteString -> Base16
sha1 = encode . D . stateToHash . sha1''

{- End SHA1 Implementation -}

keyedSHA :: Key -> ByteString -> SHA1State
keyedSHA k m = sha1'' $ k `B.append` m

{-# LANGUAGE TemplateHaskell #-}

{-
This is an implementation of MT19937. See
https://en.wikipedia.org/wiki/Mersenne_Twister for details.
-}

module Cipher.RNG where

import Data.Bifunctor
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Conversion
import Control.Lens
import Data.Maybe
import Control.Monad
import qualified Control.Monad.State.Lazy as C
import Data.Word

import Util hiding (d)
import Xor

{- Magic Numbers -}

lowerMask :: Word32
lowerMask = shiftL 1 31 - 1

upperMask :: Word32
upperMask = shiftL 1 31

rngN :: Integral a => a
rngN = 624

rngM :: Integral a => a
rngM = 397

rngR :: Integral a => a
rngR = 31

rngA :: Integral a => a
rngA = 0x9908b0df

rngU :: Integral a => a
rngU = 11

rngD :: Integral a => a
rngD = 0xFFFFFFFF

rngS :: Integral a => a
rngS = 7

rngB :: Integral a => a
rngB = 0x9d2c5680

rngT :: Integral a => a
rngT = 15

rngC :: Integral a => a
rngC = 0xefc60000

rngL :: Integral a => a
rngL = 18

rngF :: Integral a => a
rngF = 1812433253

rngW :: Integral a => a
rngW = 32

{- -}

data Twister = T { _state :: [Word32],
                   _idx   :: Int }
  deriving Show

makeLenses ''Twister

getStateVal :: Twister -> Int -> Word32
getStateVal t i = (t ^. state) !! i

-- Given a seed, inits the RNG
initRNG :: Word32 -> Twister
initRNG s = initRNG' 1 $ T (replicate rngN 0 & ix 0 .~ s) rngN

initRNG' :: Int -> Twister -> Twister
initRNG' i t@(T s i') | i == rngN = t
                      | otherwise = initRNG' (i + 1) $ T (s & ix i .~ val) i'
  where val  = rngF * val' + (fromIntegral i)
        val' = (s !! (i - 1)) `xor` (shiftR (s !! (i - 1)) (rngW - 2))

-- update state
twist :: Twister -> Twister
twist = twist' 0

twist' :: Int -> Twister -> Twister
twist' i t | i == rngN = t & idx .~ 0
           | otherwise = twist' (i + 1) (t & state %~ (& ix i .~ val))
  where temp  = (getStateVal t i .&. upperMask) +
                (getStateVal t (i + 1 `mod` rngN) .&. lowerMask)
        tempA = if temp `mod` 2 /= 0
                then (shiftR temp 1) `xor` rngA
                else shiftR temp 1
        val   = tempA `xor` (getStateVal t ((i + rngM) `mod` rngN))

-- Return 32 bits of randomness from the twister and the updated
-- twister
getRand :: Twister -> (Word32, Twister)
getRand t'@(T s' i) | i >= rngN = getRand $ twist t'
                    | otherwise = (y'''', t' & idx %~ (+1))
  where y     = s' !! i
        y'    = y `xor` ((y `shiftR` rngU) .&. rngD)
        y''   = y' `xor` ((y' `shiftL` rngS) .&. rngB)
        y'''  = y'' `xor` ((y'' `shiftL` rngT) .&. rngC)
        y'''' = y''' `xor` (y''' `shiftR` rngL)

getN :: Int -> Twister -> ([Word32], Twister)
getN 0 t = ([], t)
getN n t = first (r:) (getN (n - 1) t')
  where (r, t') = getRand t

-- Seed -> PlainText -> CipherText
cMT19937 :: Word32 -> PlainText -> CipherText
cMT19937 s = cMT19937' (initRNG s)

streamToKey :: [Word32] -> ByteString
streamToKey = B.concat . (toByteString' <$>)

cMT19937' :: Twister -> ByteString -> ByteString
cMT19937' t p = p `bXor` key
  where key = streamToKey $ fst $ getN (B.length p `div'` 4) t

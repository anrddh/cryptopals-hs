{-# LANGUAGE TemplateHaskell #-}

{-
This is an implementation of MT19937. See
https://en.wikipedia.org/wiki/Mersenne_Twister for details.
-}

module Cipher.RNG where

import Data.Bits
-- import Data.Vector.Unboxed (Vector)
-- import qualified Data.Vector.Unboxed as V
import Data.ByteString (ByteString)
import Control.Lens
import Data.Maybe
import Data.Word

import Util hiding (d)

{- Magic Numbers -}

lowerMask :: Word32
lowerMask = shiftL 1 31 - 1

upperMask :: Word32
upperMask = shiftL 1 31

n :: Integral a => a
n = 624

m :: Integral a => a
m = 397

r :: Integral a => a
r = 31

a :: Integral a => a
a = 0x9908B0DF

u :: Integral a => a
u = 11

d :: Integral a => a
d = 0xFFFFFFFF

s :: Integral a => a
s = 7

b :: Integral a => a
b = 0x9d2c5680

t :: Integral a => a
t = 15

c :: Integral a => a
c = 0xefc60000

l :: Integral a => a
l = 18

f :: Integral a => a
f = 1812433253

w :: Integral a => a
w = 32

{- -}

data Twister = T { _state :: [Word32],
                   _idx   :: Int }
  deriving Show

makeLenses ''Twister

getStateVal :: Twister -> Int -> Word32
getStateVal t i = (t ^. state) !! i

-- Given a seed, inits the RNG
initRNG :: Word32 -> Twister
initRNG s = initRNG' 1 $ T (replicate n 0 & ix 0 .~ s) n

initRNG' :: Int -> Twister -> Twister
initRNG' i t@(T s i') | i == n = t
                      | otherwise = initRNG' (i + 1) $ T (s & ix i .~ val) i'
  where val  = f * val' + (fromIntegral i)
        val' = (s !! (i - 1)) `xor` (shiftR (s !! (i - 1)) (w - 2))

-- update state
twist :: Twister -> Twister
twist = twist' 0

twist' :: Int -> Twister -> Twister
twist' i t | i == n = t & idx .~ 0
           | otherwise = twist' (i + 1) (t & state %~ (& ix i .~ val))
  where temp  = (getStateVal t i .&. upperMask) +
                (getStateVal t (i + 1 `mod` n) .&. lowerMask)
        tempA = if temp `mod` 2 /= 0
                then (shiftR temp 1) `xor` a
                else shiftR temp 1
        val   = tempA `xor` (getStateVal t ((i + m) `mod` n))

-- get 32 bits of randomness from the twister and new twister
getRand :: Twister -> (Word32, Twister)
getRand t'@(T s' i) | i >= n = getRand $ twist t'
                    | otherwise = (y'''', t' & idx %~ (+1))
  where y = s' !! i
        y' = y `xor` ((shiftR y u) .&. d)
        y'' = y' `xor` ((shiftL y' s) .&. b)
        y''' = y'' `xor` ((shiftL y'' t) .&. c)
        y'''' = y''' `xor` (shiftR y''' l)

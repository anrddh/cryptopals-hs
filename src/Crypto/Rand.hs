module Crypto.Rand where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad
import System.Random
import Data.Word

-- Generates a random ByteString that's n bytes long
genBytes :: Int -> IO ByteString
genBytes n = replicateM n randomIO
             >>= pure . B.pack

-- Generates a random key
genKey :: IO ByteString
genKey = genBytes 16

getPosNum :: IO Int
getPosNum = randomRIO (1, 512)

-- Returns a number between 5 and 10
getCount :: IO Int
getCount = randomRIO (4, 11)

-- Returns two random ByteStrings of length between 5 and 10
genPlaintexts :: IO (ByteString, ByteString)
genPlaintexts = do x  <- getCount
                   y  <- getCount
                   x' <- genBytes x
                   y' <- genBytes y
                   pure (x', y')

-- Generates a random bool given a seed
genBool :: IO Bool
genBool = do gen <- getStdGen
             let (num, gen') = randomR (0 :: Int, 1) gen
             setStdGen gen'
             pure $ case num of
                      0 -> True
                      1 -> False


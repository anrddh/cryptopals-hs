module S3.C22 where

import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent
import Data.Word

import Cipher.RNG

-- seconds
minTime :: Integral a => a
minTime = 40

-- seconds
maxTime :: Integral a => a
maxTime = 60

getRandom :: IO Word32
getRandom = do x <- randomRIO (minTime, maxTime)
               threadDelay $ x * 1000000
               t <- round <$> getPOSIXTime
               putStrLn $ "Generated seed: " ++ (show t)
               let rng = initRNG t
               y <- randomRIO (minTime, maxTime)
               threadDelay $ y * 1000000
               pure $ fst $ getRand rng

-- First output -> range of seeds
bruteForceSeed :: Word32 -> [Word32] -> Word32
bruteForceSeed o r = snd . head $
                     filter ((==o) . fst) $
                     map (fst . getRand . initRNG >>= (,)) r

runS3C22 :: IO ()
runS3C22 = do r <- getRandom
              t <- round <$> getPOSIXTime
              putStrLn $ "Bruteforced seed: " ++ (show $ bruteForceSeed r [(t-(3 * maxTime))..t])

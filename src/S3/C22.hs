module S3.C22 where

import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent
import Data.Word

import Cipher.RNG

getRandom :: IO Word32
getRandom = do x <- randomRIO (40 :: Int, 1000)
               threadDelay x
               t <- (round . (* 1000)) <$> getPOSIXTime
               putStrLn $ "Generated seed: " ++ (show t)
               let rng = initRNG t
               y <- randomRIO (40 :: Int,1000)
               threadDelay y
               pure $ fst $ getRand rng

-- First output -> range of seeds
bruteForceSeed :: Word32 -> [Word32] -> Word32
bruteForceSeed o r = head $ filter (==o) $ map (fst . getRand . initRNG) r

runS3C22 :: IO ()
runS3C22 = do r <- getRandom
              t <- (round . (* 1000)) <$> getPOSIXTime
              putStrLn $ "Bruteforced seed: " ++ (show $ bruteForceSeed r [t-5000000..t])

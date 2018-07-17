module S3.C24 where

import qualified Data.ByteString as B
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random
import Data.Word

import Cipher.Rand
import Cipher.RNG
import Util

encrypt :: PlainText -> IO CipherText
encrypt pt = do key    <- genKey
                num    <- randomRIO (1, 30)
                seed   <- (randomIO :: IO Word16)
                prefix <- genBytes num
                putStrLn $ "Generated seed: " ++ show seed
                let newPt      = prefix `B.append` pt
                    ciphertext = cMT19937 (fromIntegral seed) newPt
                pure ciphertext

-- Given a plaintext, and the corresponding ciphertext (with an
-- unknown prefix prepended to the plaintext) encrypted with a trivial
-- M19937 stream cipher, brute force the seed that generated the
-- keystream.
-- _Very slow_, but works.
breakSeed :: PlainText -> CipherText -> Word16
breakSeed pt ct = fromIntegral $ head $ map fst $
                  filter (\(_, b) -> b == relCt) cts
  where prefixLen = B.length ct - B.length pt
        newPt     = charRepl prefixLen 5 `B.append` pt
        seeds     = [0..0xffff]
        relCt     = B.drop prefixLen ct
        cts       = ((,) <*> (B.drop prefixLen . flip cMT19937 newPt)) <$> seeds

genToken :: Word32 -> Word32
genToken = last . fst . getN 5 . initRNG

tokenSeedTime :: IO Word32
tokenSeedTime = round <$> getPOSIXTime >>= pure . genToken

isGenByTime :: Word32 -> IO Bool
isGenByTime tok = do time <- round <$> getPOSIXTime
                     pure $ tok `elem` (genToken <$> [(time - 300)..time])

runS3C24 :: IO ()
runS3C24 = do let pt = charRepl 14 65
              ct <- encrypt pt
              putStrLn $ "Cracked seed: " ++ show (breakSeed pt ct)
              putStrLn . show =<< isGenByTime =<< tokenSeedTime

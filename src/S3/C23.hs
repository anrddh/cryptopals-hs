module S3.C23 where

import Data.Bits
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word

import Cipher.RNG

untemper4 :: Word32 -> Word32
untemper4 x = f18 + (shiftR f18 18 `xor` (x .&. 0x3fff))
  where f18 = x .&. 0xffffc000

untemper3 :: Word32 -> Word32
untemper3 x = l15 + sl15 + f2
  where l15   = x .&. 0x7fff
        sl15x = x .&. 0x3fff8000
        sl15  = sl15x `xor` ((l15 `shiftL` 15) .&. rngC .&. 0x3fff8000);
        f2x   = x .&. 0xc0000000
        f2    = f2x `xor` (((sl15 .&. 0x18000) `shiftL` 15) .&. rngC .&. 0xc0000000);

untemper2 :: Word32 -> Word32
untemper2 x = l7 + sl7 + tl7 + fl7 + f4
  where l7   = x .&. 0x7f
        sl7x = x .&. 0x3f80
        sl7  = sl7x `xor` ((l7 `shiftL` 7) .&. rngB .&. 0x3f80)
        tl7x = x .&. 0x1fc000
        tl7  = tl7x `xor` ((sl7 `shiftL` 7) .&. rngB .&. 0x1fc000)
        fl7x = x  .&. 0xfe00000
        fl7  = fl7x `xor` ((tl7 `shiftL` 7) .&. rngB .&. 0xfe00000)
        f4   = (x .&. 0xf0000000) `xor` ((fl7 `shiftL` 7) .&. rngB .&. 0xf0000000)

untemper1 :: Word32 -> Word32
untemper1 x = f11 + s11 + l10
  where f11 = x .&. 0xffe00000
        s11 = (x .&. 0x1ffc00) `xor` (f11 `shiftR` 11)
        l10 = (s11 `shiftR` 11) `xor` (x .&. 0x3ff)

-- Given a num, return the 
untemper :: Word32 -> Word32
untemper = untemper1 . untemper2 . untemper3 . untemper4

runS3C23 :: IO ()
runS3C23 = do time <- (round . (* 1000)) <$> getPOSIXTime
              let t     = initRNG time
                  state = untemper <$> (fst $ getN 624 t)
                  t'    = T state 0
              putStrLn $ show (fst $ getN 10 t)
              putStrLn $ show (fst $ getN 10 t')

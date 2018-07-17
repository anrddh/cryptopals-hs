module S4.C25 where

import Control.Lens
import Data.Word

import Cipher.AES
import Util

-- A more efficient implementation would only decrypt/encrypt the
-- block we care about, but what the hell.
edit :: Nonce -> Key -> Int -> CipherText -> Word8 -> CipherText
edit n k pos ct b = eCTR k n 0 edited
  where decrypted = dCTR k n 0 ct
        edited    = decrypted & ix pos .~ b

runS4C25 :: IO ()
runS4C25 = undefined

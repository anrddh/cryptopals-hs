{-# LANGUAGE OverloadedStrings #-}

module S4.C25 where

import Prelude hiding (break)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Lens
import Data.Word
import Data.Text.Encoding
import Data.Maybe

import Crypto.AES
import Crypto.Rand
import Util
import Xor

edit :: Nonce -> Key -> Int -> CipherText -> Word8 -> CipherText
edit n k pos ct b = B.take blockStart ct `B.append`
                    encrypted `B.append`
                    B.drop (blockStart + 16) ct
  where blockNum       = pos `div` 16
        blockStart     = blockNum * 16
        decryptedBlock = ctrBlock k n blockNum (B.drop blockStart ct)
        edited         = decryptedBlock & ix (pos - blockStart) .~ b
        encrypted      = ctrBlock k n blockNum edited

editCiphertext :: (Int -> CipherText -> Word8 -> CipherText)
               -> Int -- curr offset
               -> CipherText
               -> Word8
               -> CipherText
editCiphertext e i c w
  | B.length c == i = c
  | otherwise       = editCiphertext e (i + 1) (e i c w) w

break :: (Int -> CipherText -> Word8 -> CipherText)
      -> CipherText
      -> PlainText
break e c = c `bXor` editedCt `bXor` (charRepl (B.length c) 65)
  where editedCt = editCiphertext e 0 c 65

runS4C25 :: IO ()
runS4C25 = do key  <- genKey
              file <- fromJust . stripPad . dECB "YELLOW SUBMARINE" . d . right . decode . B64 <$> (B.readFile "25.txt")
              putStrLn . show $ break (edit 0 key) (eCTR key 0 0 file)
  where right (Right x) = x

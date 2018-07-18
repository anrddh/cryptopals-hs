{-# LANGUAGE OverloadedStrings #-}

module S4.C27 where

import qualified Data.ByteString as B
import Data.Maybe
import Control.Monad
  
import Crypto.AES
import Crypto.Rand
import Util
import Xor

-- Only works correctly if input is padded
eCBC' :: Key -> PlainText -> CipherText
eCBC' = join eCBC

dCBC' :: Key -> CipherText -> PlainText
dCBC' = join dCBC

verifyMessage :: Key -> CipherText -> Maybe PlainText
verifyMessage k c = case B.any (>127) decrypted of
                      True  -> Just decrypted
                      False -> Nothing
  where decrypted = dCBC' k c

getKey :: (PlainText -> CipherText)
       -> (CipherText -> Maybe PlainText)
       ->  Key
getKey encrypt verify = verBlock1 `bXor` verBlock3
  where message   = charRepl 48 68
        encrypted = encrypt message
        encBlock1 = B.take 16 encrypted
        modified  = encBlock1 `B.append` charRepl 16 0 `B.append` encBlock1
        verified  = fromJust $ verify modified
        verBlock1 = B.take 16 verified
        verBlock3 = B.drop 32 verified

runS4C27 :: IO ()
runS4C27 = do key <- genKey
              putStrLn $ "Generated key:\t" ++ show key
              putStrLn $ "Cracked key:\t" ++ show (getKey (eCBC' key) (verifyMessage key))

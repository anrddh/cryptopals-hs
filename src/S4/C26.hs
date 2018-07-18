{-# LANGUAGE OverloadedStrings #-}

module S4.C26 where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Lens
import Data.Maybe
import Data.Word

import Crypto.AES
import Crypto.Rand
import Util

prefix :: ByteString
prefix = "comment1=cooking MCs;userdata="

postfix :: ByteString
postfix = ";comment2= like a pound of bacon"

convert :: (Enum a, Num b) => a -> b
convert = fromIntegral . fromEnum

filterBadChars :: ByteString -> ByteString
filterBadChars = B.filter $ liftA2 (&&) (convert ';' /=) (convert '&' /=)

encrypt :: Key -> Nonce -> PlainText -> CipherText
encrypt k n x = eCTR k n 0 (genString x)
  where genString x = prefix `B.append` filterBadChars x `B.append` postfix

decrypt :: Key -> Nonce -> CipherText -> PlainText
decrypt k n x = dCTR k n 0 x

isAdmin :: PlainText -> Bool
isAdmin = B.isInfixOf ";admin=true;"

genFuncs :: Key -> Nonce -> (PlainText -> CipherText, CipherText -> PlainText)
genFuncs k n = (encrypt k n, decrypt k n)

-- Given a CipherText and a position, change the CipherText byte until
-- the decrypted value is equal to the third argument.
bruteForceByte :: (CipherText -> PlainText)
               -> CipherText
               -> Int
               -> Word8
               -> CipherText
bruteForceByte dec ct pos b =
  head $ map (\b' -> ct & ix pos .~ b') $ filter filtByte [0..255]
  where filtByte b' = fromJust ((dec (ct & ix pos .~ b')) ^? ix pos) == b

runS4C26 :: IO ()
runS4C26 = do
  key <- genBytes 16
  let (enc, dec) = genFuncs key 0
  let ct = enc "AAAadminAtrue"
  putStrLn $ show $ bruteForceByte dec (bruteForceByte dec ct 32 59) 38 61 -- 59 ~ ';', 61 ~ '='

{-# LANGUAGE OverloadedStrings #-}

module S2.C16 where

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

encrypt :: Key -> PlainText -> CipherText
encrypt k x = eCBC k (charRepl 16 0) (padded x)
  where genString x = prefix `B.append` filterBadChars x `B.append` postfix
        padded      = flip pad' 16 . genString

decrypt :: Key -> CipherText -> PlainText
decrypt k x = fromJust $ stripPad $ dCBC k (charRepl 16 0) x

isAdmin :: PlainText -> Bool
isAdmin = B.isInfixOf ";admin=true;"

-- Given a CipherText and a position, change the CipherText byte until
-- the decrypted value is equal to the third argument.
bruteForceByte :: (CipherText -> PlainText)
               -> CipherText
               -> Int
               -> Word8
               -> CipherText
bruteForceByte dec ct pos b =
  head $ map (\b' -> ct & ix actualPos .~ b') $ filter filtByte [0..255]
  where actualPos   = pos - 16
        filtByte b' = fromJust ((dec (ct & ix actualPos .~ b')) ^? ix pos) == b

genFuncs :: Key -> (PlainText -> CipherText, CipherText -> PlainText)
genFuncs k = (encrypt k, decrypt k)

runS2C16 :: IO ()
runS2C16 = do
  key <- genBytes 16
  let (enc, dec) = genFuncs key
  let ct = enc "AAAadminAtrue"
  putStrLn $ show $ bruteForceByte dec (bruteForceByte dec ct 32 59) 38 61 -- 59 ~ ';', 61 ~ '='

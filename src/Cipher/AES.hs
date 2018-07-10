module Cipher.AES where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Crypto.Cipher.AES

-- Key -> PlainText -> CipherText
eECB :: ByteString -> ByteString -> ByteString
eECB = encryptECB . initAES

-- Key -> CipherText -> PlainText
dECB :: ByteString -> ByteString -> ByteString
dECB = decryptECB . initAES

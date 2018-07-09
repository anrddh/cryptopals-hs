module Cipher where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text(Text)

import Util
import Xor

-- Key -> PlainText -> CipherText
repeatkey :: Decoded -> Decoded -> Decoded
repeatkey (D k) (D pt) = D $ BL.toStrict $
                         BL.cycle (BL.fromStrict k) `lXor` (BL.fromStrict pt)

-- repeat key for encoded strings
repeatKeyEnc :: (Encodable a, Encodable b, Encodable c)
             => a
             -> b
             -> Either Text c
repeatKeyEnc keyE ptE = decode keyE
                        >>= \key -> decode ptE
                                    >>= pure . encode . repeatkey key

editDist :: Decoded -> Decoded -> Int
editDist (D h1) (D h2) = sum $ popCount <$> (B.unpack $ h1 `bXor` h2)

{-# LANGUAGE OverloadedStrings #-}

module S2.C14 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Safe
import Data.Word

import Crypto.AES
import Crypto.Rand
import Util

type Oracle = PlainText -> CipherText

magicString :: ByteString
magicString = d $ right $ decode $ B64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
  where right (Right x) = x

consOracle :: Key -> ByteString -> Oracle
consOracle key pre pt = eECB key msg
  where msg = pre `B.append` pt `B.append` magicString

initOracle :: IO Oracle
initOracle = do n <- getPosNum
                pre <- genBytes n -- Random Prefix
                key <- genKey     -- Random Key
                pure $ consOracle key pre

-- The solution here is essentially the same as the solution for
-- Challenge 12.  We just need to figure out the length of the prefix
-- and use that to adjust the length of our attack payload.

-- Given an Oracle, determine the number of blocks that the prefix
-- occupies (with an extra block, potentially)
prefixBlocks :: Oracle -> Int
prefixBlocks o = prefixLen (o "") (o "A") `div'` 16 + 1

-- Given an Oracle, and the number of blocks occupied by the prefix,
-- determine the number of bytes used by the prefix in the last block
prefixLastBlock :: Oracle -> Int -> Int
prefixLastBlock = prefixLastBlock' 0

-- :::  NumBytes -> Oracle -> NumBlocks -> NumBytes
prefixLastBlock' :: Int -> Oracle -> Int -> Int
prefixLastBlock' 16 _ _ = 0
prefixLastBlock' n o nB = if get n == get (n + 1)
                          then 16 - n
                          else prefixLastBlock' (n + 1) o nB
  where get n' = B.take (nB * 16) (o (charRepl n' 65))

-- Given an Oracle, determine the length of the generated prefix
lengthPrefix :: Oracle -> Int
lengthPrefix o = ((numBlocks - 1) * 16) + lastBlock
  where numBlocks = prefixBlocks o
        lastBlock = prefixLastBlock o numBlocks

-- Given an oracle, the current block number (which represents the
-- current block of
-- Oracle -> CurrBlock -> Input -> Byte
bruteForceByte :: Oracle -> Int -> ByteString -> ByteString -> Maybe Word8
bruteForceByte o b c i = headMay $
                         filter
                         ((getBlks b (o i) ==) . getBlks b . o . B.snoc (i `B.append` c))
                         [0..255]
  where getBlks = B.take . (* 16)

postfix :: Oracle -> ByteString
postfix = flip postfix' ""

-- This is mostly a wrapper around bruteForceByte, which does all the
-- work.
-- Oracle -> Current -> Rest
postfix' :: Oracle -> ByteString -> ByteString
postfix' o c = case bruteForceByte o numBlocks c inp of
                 Just b  -> postfix' o $ c `B.snoc` b
                 Nothing -> c
  where currLen      = B.length c                       -- Length of the (partially) computed str
        prefixLen    = lengthPrefix o
        preLastBlock = prefixLastBlock o (prefixBlocks o)
        numBlocks    = (currLen + prefixLen) `div` 16 + 1
        currBlock    = (currLen + preLastBlock) `div` 16 + 1
        inpLen       = currBlock * 16 - currLen - 1 - preLastBlock
        inp          = charRepl inpLen 65

runS2C14 :: IO ()
runS2C14 = do oracle <- initOracle -- Get a random oracle
              let l = lengthPrefix oracle
              undefined

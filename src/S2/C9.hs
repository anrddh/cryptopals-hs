{-# LANGUAGE OverloadedStrings #-}

module S2.C9 where

import Prelude hiding (putStr, putStrLn)

import qualified Data.ByteString as B
import Control.Monad
import Data.Text
import Text.Read
import Data.Text.IO
import Data.Text.Encoding
import System.IO (hFlush, stdout)

import Util
import Cipher

runS2C9 :: IO ()
runS2C9 = do pt <- D <$> B.getLine
             putStr "Enter pad: "
             hFlush stdout
             numM <- readMaybe <$> Prelude.getLine
             case numM of
               Just n  -> putStrLn $ decodeUtf8 $ pad (d pt) n
               Nothing -> putStrLn "Need num"

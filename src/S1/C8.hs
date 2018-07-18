{-# LANGUAGE OverloadedStrings #-}

module S1.C8 where

import Prelude hiding (putStr, putStrLn)

import qualified Data.ByteString as B
import Control.Monad
import Data.Text hiding (map, filter, head)
import Data.Text.IO
import Data.Text.Encoding

import Util
import Crypto.AES

runS1C8 :: IO ()
runS1C8 = do file <- readHexFile "c8.txt"
             case sequence $ decode <$> file of
               Left _   -> putStrLn "Error"
               Right ds -> putStrLn $ pretty $ (encode $ D $ fst $ head $ filter (\(_, x) -> x) $ [ (x, isECB x) | x <- d <$> ds ] :: Base16)
               -- Right ds -> print $ Prelude.filter id $ isECB . d <$> ds

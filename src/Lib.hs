module Lib
    ( run
    ) where

import Text.Read
import System.IO (hFlush, stdout)

import C1
import C2
import C3
import C4

run :: IO ()
run = putStr "Enter challenge number: "
      *>  hFlush stdout
      *>  getLine
      >>= \line -> case readMaybe line of
                     Just 1 -> runS1C1
                     Just 2 -> runS1C2
                     Just 3 -> runS1C3
                     Just 4 -> runS1C4
                     _      -> putStrLn "Bad input." *> run

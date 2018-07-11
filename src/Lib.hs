module Lib
    ( run
    ) where

import Text.Read
import System.IO (hFlush, stdout)

import S1.C1
import S1.C2
import S1.C3
import S1.C4
import S1.C5
import S1.C6
import S1.C7
import S1.C8
import S2.C9
import S2.C10
import S2.C11

run :: IO ()
run = putStr "Enter challenge number: "
      *>  hFlush stdout
      *>  getLine
      >>= \line -> case readMaybe line of
                     Just 1  -> runS1C1
                     Just 2  -> runS1C2
                     Just 3  -> runS1C3
                     Just 4  -> runS1C4
                     Just 5  -> runS1C5
                     Just 6  -> runS1C6
                     Just 7  -> runS1C7
                     Just 8  -> runS1C8
                     Just 9  -> runS2C9
                     Just 10 -> runS2C10
                     Just 11 -> runS2C11
                     _       -> putStrLn "Bad input." *> run

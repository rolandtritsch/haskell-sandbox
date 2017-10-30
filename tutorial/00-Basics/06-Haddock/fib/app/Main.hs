{-|
Module      : Main
Description : Main module to calc the Fibonacci sequence
Copyright   : (c) Roland Tritsch, 2017
License     : BSD
Maintainer  : roland@tritsch.org
Stability   : experimental
Portability : POSIX

The main purpose of this module (and all the other
modules in this project) is to play with [Haddock](https://www.haskell.org/haddock/).

Command line syntax:
@
  fib <level>
@
-}
module Main where

import Fibonacci
import System.Environment (getArgs)

-- | The main function to run the 'fib' function
main :: IO ()
main = do
  args <- getArgs
  let level = read (args !! 0) :: Int
  putStrLn $ show (fib level)

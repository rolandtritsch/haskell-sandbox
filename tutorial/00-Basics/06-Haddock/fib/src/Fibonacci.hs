{-|
Module      : Fibonacci
Description : An implementation of the Fibonacci sequence
Copyright   : (c) Roland Tritsch, 2017
License     : BSD
Maintainer  : roland@tritsch.org
Stability   : experimental
Portability : POSIX

This module implements a 'fib' function
-}
module Fibonacci (
  fib
) where

import Control.Exception (assert)

-- | Calc the fib seq for input n
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = do
  let n' = assert (n >= 2) n
  fib (n' - 1) + fib (n' - 2)

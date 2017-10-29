-- Trace.hs
module Trace where

import Debug.Trace

example1 :: Int
example1 = trace "impure print" 1

example2 :: Int
example2 = traceShow "tracing" 2

example3 :: [Int]
example3 = [trace "will not be called" 3]

main :: IO ()
main = do
  print example1
  print example2
  print $ length example3
  print (length example3)

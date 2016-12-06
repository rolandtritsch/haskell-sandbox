-- Fibonacci.hs
module Fibonacci where

fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

fibs = 1 : scanl (+) 1 fibs
fibsN n = fibs !! n

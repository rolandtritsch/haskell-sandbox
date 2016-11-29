-- DividedBy.hs
module DividedBy where

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

dividedBy :: Numerator -> Denominator -> (Quotient, Remainder)
dividedBy num denom = go num denom 0 where
  go n d c
    | n < d = (c, n)
    | otherwise = go (n - d) d (c + 1)

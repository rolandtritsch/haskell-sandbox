-- Josephus.hs
module Josephus where

-- pre: 1 < n < k
jos 1 k = 0
jos n k = mod ((jos (n - 1) k) + k) n

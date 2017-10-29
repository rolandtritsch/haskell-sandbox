-- Bottoms.hs
module Bottoms where

data F = A | B

check :: F -> ()
check f = case f of
  A -> ()

{-# LANGUAGE RecursiveDo #-}

-- Do.hs
module Do where

justOnes :: [Integer] -> [Integer]
justOnes = do
  rec xs <- justOnes (1: xs)
  return (map negate xs)

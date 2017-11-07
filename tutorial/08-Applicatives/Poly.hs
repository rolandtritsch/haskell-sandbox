{-# LANGUAGE FlexibleInstances #-}

-- Poly.hs
module Poly where

class Arg a where
  collect' :: [String] -> a

instance Arg (IO ()) where
  collect' acc = mapM_ putStrLn acc

instance Arg [String] where
  collect' acc = acc

instance (Show a, Arg r) => Arg (a -> r) where
  collect' acc = \x -> collect' (acc ++ [show x])

collect :: Arg t => t
collect = collect' []

example1 :: [String]
example1 = collect 'a' 2 3.0

example2 :: IO ()
example2 = collect () "foo" [1, 2, 3]

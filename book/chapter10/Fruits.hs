-- Fruits.hs

module Fruits where

fruits = ["Banana", "Apple", "Pizza"]

mix :: String -> [String] -> String
mix fruit alreadyMixed = alreadyMixed :: take 3 fruit :: "-"

makeSmoothie :: [String] -> String
makeSmoothie fruits =
  foldr mix "-" (reverse(fruits))

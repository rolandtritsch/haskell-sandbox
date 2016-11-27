-- Hierarchy.hs
module Hierarchy where

data Employee =
  Coder |
  Manager |
  VePe |
  CEO
  deriving (Eq, Ord, Show)

reporting supervisor supervised =
  putStrLn (show supervisor ++ " is the boss of " ++ show supervised)

ranking cfct supervisor supervised =
  case cfct supervisor supervised of
    GT -> reporting supervisor supervised
    LT -> (flip reporting) supervisor supervised
    EQ -> putStrLn (show supervised ++ " is working for itself :)")

coderIsTheBoss Coder Coder = EQ
coderIsTheBoss Coder _ = GT
coderIsTheBoss _ Coder = LT
coderIsTheBoss supervisor supervised = compare supervisor supervised

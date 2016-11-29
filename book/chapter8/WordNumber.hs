-- WordNumber.hs
module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = words !! n where
  words = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digits :: Int -> [Int]
digits n = go n [] where
  go n c
    | n == 0 = c
    | otherwise = go (div n 10) ((mod n 10) : c)

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))

wordNumber2 :: Int -> String
wordNumber2 n = concat . intersperse "-" . map digitToWord . digits $ 123

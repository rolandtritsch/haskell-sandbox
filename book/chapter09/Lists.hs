-- Lists.hs
module Lists where

myWords :: String -> [String]
myWords s = go s [] where
  go s c
    | null s = reverse c
    | elem ' ' s = go ((tail . dropWhile (/=' ')) s) ((takeWhile (/=' ') s) : c)
    | otherwise = go (dropWhile (/=' ') s) ((takeWhile (/=' ') s) : c)

myAc :: String -> String
myAc xs = [x | x <- xs, elem x ['A' .. 'Z']]

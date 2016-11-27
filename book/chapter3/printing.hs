-- Printing.hs
module Printing where

myGreeting :: String
myGreeting = "hello" ++ " " ++ "roland!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO()
main = do
  putStrLn myGreeting
  putStrLn stdGreeting
  where stdGreeting = concat[hello, " ", world]

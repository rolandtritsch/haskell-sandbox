-- SayHello.hs
module SayHello where

sayHello :: String -> IO()
sayHello x = putStrLn("Hello " ++ x ++ "!")

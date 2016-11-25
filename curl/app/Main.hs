module Main where

import Lib
import Network.HTTP.Client

main :: IO ()
main =

manager <- newManager defaultManagerSettings
request <- parseRequest arg1
response <- httpLbs request manager
putStrLn (read (show (responseBody response)))

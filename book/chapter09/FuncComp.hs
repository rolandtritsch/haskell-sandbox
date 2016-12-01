-- FuncComp.hs
module FuncComp where

-- oneWay [1..3]
oneWay xs = take 2 (map (*2) xs)
andAnotherOne xs = take 2 $ map (*2) xs
andAnotherOne2 xs = (take 2 . map (*2)) xs
andAnotherOne3 = take 2 . map (*2)

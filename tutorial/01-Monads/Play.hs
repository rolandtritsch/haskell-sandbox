-- Play.hs
module Play where

example :: [(Integer, Integer, Integer)]
example = do
  a <- [1, 2]
  b <- [10, 20]
  c <- [100, 200]
  return (a, b, c)

example2 :: IO ()
example2 = do
  putStr "What's your name: "
  name <- getLine
  putStrLn name

example3 :: IO ()
example3 = putStr "What's your name again: " >>=
  \_ -> getLine >>=
  \name -> putStrLn name


addMaybe :: (Num b, Monad m) => m b -> m b -> m b
addMaybe x y = do
  a <- x
  b <- y
  return $ a + b

example4 :: Maybe Int
example4 = addMaybe (Just 3) (Just 4)

example5 :: Maybe Int
example5 = addMaybe (Just 3) Nothing

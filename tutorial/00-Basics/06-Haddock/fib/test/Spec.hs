{-|
Module      : Test
Description : Testing the/my fib sequence implementation
Copyright   : (c) Roland Tritsch, 2017
License     : BSD
Maintainer  : roland@tritsch.org
Stability   : experimental
Portability : POSIX

Testing a/the fib seq implementation with [Hspec](https://hspec.github.io/).
-}

import Fibonacci

import Test.Hspec
--import Control.Exception (evaluate)

-- | The main function to run all tests
main :: IO ()
main = hspec $ do
  describe "fib" $ do
    it "should return 0 for input 0" $ do
      fib 0 `shouldBe` 0

    it "should return 1 for input 1" $ do
      fib 1 `shouldBe` 1

    it "should return 1 for input 2" $ do
      fib 2 `shouldBe` 1

    it "should return 55 for input 10" $ do
      fib 10 `shouldBe` 55

    it "should return 6765 for input 20" $ do
      fib 20 `shouldBe` 6765

    --it "should return 0 for input 100" $ do
      --fib 50 `shouldBe` 12586269025

    --it "should throw an exception for inputs < 0" $ do
      --evaluate (fib (-1)) `shouldThrow` anyException

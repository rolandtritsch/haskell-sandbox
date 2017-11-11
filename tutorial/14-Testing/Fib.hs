{-# LANGUAGE BangPatterns #-}

-- Fib.hs
module Fib where

import Data.List
import Data.Maybe
import Data.Bits

import Criterion.Main
import Control.Monad.State
import Control.Exception

import Test.Hspec

data InvalidParameterException = InvalidParameterException deriving (Show)
instance Exception InvalidParameterException

fib1 :: Int -> Int
fib1 0 = 0
fib1 1 = 1
fib1 n = assert (n >= 2) (fib1 (n - 1) + fib1 (n - 2))

fib2 :: Int -> Int
fib2 n = assert (n >= 0) truncate $ (1 / sqrt 5) * (phi^n - psi^n) where
  phi = (1 + sqrt 5) / 2
  psi = (1 - sqrt 5) / 2

fib3 :: Int -> Int
fib3 n = assert (n >= 0) fibs !! n where
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib4 :: Int -> Int
fib4 n = assert (n >= 0) go n (0, 1) where
  go !n (!a, !b)
    | n == 0 = a
    | otherwise = go (n - 1) (b, a + b)

fib5 :: Int -> Int
fib5 n = assert (n >= 0) flip evalState (0, 1) $ do
  forM [0 .. (n - 1)] $ \_ -> do
    (a, b) <- get
    put (b, a + b)
  (a, b) <- get
  return a

fib6 :: Int -> Int
fib6 n = assert (n >= 0) fibs !! n where
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib7 :: Int -> Int
fib7 n = assert (n >= 0) fibs !! n where
  fibs = 0 : 1 : next fibs where
    next (a : t@(b:_)) = (a + b) : next t

fib8 :: Int -> Int
fib8 n = assert (n >= 0) fibs !! n where
  fibs = scanl (+) 0 (1 : fibs)

fib9 :: Int -> Int
fib9 n = assert (n >= 0) fibs !! n where
  fibs = 0 : scanl (+) 1 fibs

fib10 :: Int -> Int
fib10 n = assert (n >= 0) fibs !! n where
  fibs = fix (scanl (+) 0 . (1:))

fib11 :: Int -> Int
fib11 n = assert (n >= 0) fibs !! n where
  fibs = fix ((0:) . scanl (+) 1)

fib12 :: Int -> Int
fib12 n = assert (n >= 0) fibs !! n where
  fibs = unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1)

fib13 :: Int -> Int
fib13 n = assert (n >= 0) fibs !! n where
  fibs = map fst $ iterate (\(a, b) -> (b, (a + b))) (0, 1)

fib14 :: Int -> Int
fib14 0 = 0
fib14 1 = 1
fib14 n
  | n < 0 = throw InvalidParameterException
  | even n = f1 * (f1 + 2 * f2)
  | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
  | otherwise = (2 * f1 + f2) * (2 * f1 - f2) - 2
  where
    k = n `div` 2
    f1 = fib14 k
    f2 = fib14 (k - 1)

newtype Matrix a = Matrix [[a]] deriving (Eq, Show)
instance Num a => Num (Matrix a) where
  Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
  Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
  Matrix as * Matrix bs = Matrix [[sum $ zipWith (*) a b | b <- transpose bs] | a <- as]
  negate (Matrix as) = Matrix (map (map negate) as)
  fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
  abs m = m
  signum _ = 1

apply :: Num a => Matrix a -> [a] -> [a]
apply (Matrix as) b = [sum (zipWith (*) a b) | a <- as]

fib15 :: Int -> Int
fib15 n = assert (n >= 0) head (apply (Matrix [[0, 1], [1, 1]] ^ n) [0, 1])

fib16 :: Int -> Int
fib16 0 = 0
fib16 n
  | n < 0 = throw InvalidParameterException
  | otherwise = (fst . fib) (n - 1) where
    fib 0 = (1, 1)
    fib 1 = (1, 2)
    fib n
      | even n = (a*a + b*b, c*c - a*a)
      | otherwise = (c*c - a*a, b*b + c*c)
      where
        (a, b) = fib (n `div` 2 - 1)
        c = a +b

fib17 :: Int -> Int
fib17 n = assert (n >= 0) snd . foldl fib (1, 0) . map (toEnum . fromIntegral) $ unfoldl divs n where
  unfoldl f x = case f x of
    Just (u, v) -> unfoldl f v ++ [u]
    Nothing -> []

  divs 0 = Nothing
  divs k = Just (uncurry (flip (,)) (k `divMod` 2))

  fib (f, g) p
    | p = (f * (f + 2*g), ss)
    | otherwise = (ss, g * (2*f - g))
    where
      ss = f*f + g*g

fib18 :: Int -> Int
fib18 n = assert (n >= 0) snd . foldl_ fib_ (1, 0) . dropWhile not $ [testBit n k | k <- let s = finiteBitSize n in [s - 1, s - 2..0]] where
  fib_ (f, g) p
    | p = (f * (f + 2*g), ss)
    | otherwise = (ss, g * (2*f - g))
    where
      ss = f*f + g*g

  foldl_ = foldl' --

fib19 :: Int -> Int
fib19 n = round $ phi ** fromIntegral n / sq5 where
  sq5 = sqrt 5 :: Double
  phi = (1 + sq5) / 2

runTest :: IO ()
runTest = hspec $ do
  describe "fib1" $ do
    it "should return 0, for fib 0" $ do
      fib1 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib1 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib1 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib1 (negate 1)) `shouldThrow` anyException

  describe "fib2" $ do
    it "should return 0, for fib 0" $ do
      fib2 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib2 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib2 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib2 (negate 1)) `shouldThrow` anyException

  describe "fib3" $ do
    it "should return 0, for fib 0" $ do
      fib3 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib3 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib3 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib3 (negate 1)) `shouldThrow` anyException

  describe "fib4" $ do
    it "should return 0, for fib 0" $ do
      fib4 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib4 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib4 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib4 (negate 1)) `shouldThrow` anyException

  describe "fib5" $ do
    it "should return 0, for fib 0" $ do
      fib5 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib5 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib5 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib5 (negate 1)) `shouldThrow` anyException

  describe "fib6" $ do
    it "should return 0, for fib 0" $ do
      fib6 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib6 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib6 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib6 (negate 1)) `shouldThrow` anyException

  describe "fib7" $ do
    it "should return 0, for fib 0" $ do
      fib7 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib7 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib7 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib7 (negate 1)) `shouldThrow` anyException

  describe "fib8" $ do
    it "should return 0, for fib 0" $ do
      fib8 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib8 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib8 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib8 (negate 1)) `shouldThrow` anyException

  describe "fib9" $ do
    it "should return 0, for fib 0" $ do
      fib9 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib9 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib9 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib9 (negate 1)) `shouldThrow` anyException

  describe "fib10" $ do
    it "should return 0, for fib 0" $ do
      fib10 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib10 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib10 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib10 (negate 1)) `shouldThrow` anyException

  describe "fib11" $ do
    it "should return 0, for fib 0" $ do
      fib11 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib11 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib11 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib11 (negate 1)) `shouldThrow` anyException

  describe "fib12" $ do
    it "should return 0, for fib 0" $ do
      fib12 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib12 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib12 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib12 (negate 1)) `shouldThrow` anyException

  describe "fib13" $ do
    it "should return 0, for fib 0" $ do
      fib13 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib13 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib13 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib13 (negate 1)) `shouldThrow` anyException

  describe "fib14" $ do
    it "should return 0, for fib 0" $ do
      fib14 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib14 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib14 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib14 (negate 1)) `shouldThrow` anyException

  describe "fib15" $ do
    it "should return 0, for fib 0" $ do
      fib15 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib15 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib15 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib15 (negate 1)) `shouldThrow` anyException

  describe "fib16" $ do
    it "should return 0, for fib 0" $ do
      fib16 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib16 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib16 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib16 (negate 1)) `shouldThrow` anyException

  describe "fib17" $ do
    it "should return 0, for fib 1" $ do
      fib17 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib17 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib17 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib17 (negate 1)) `shouldThrow` anyException

  describe "fib18" $ do
    it "should return 0, for fib 1" $ do
      fib18 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib18 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib18 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib18 (negate 1)) `shouldThrow` anyException

  describe "fib19" $ do
    it "should return 0, for fib 1" $ do
      fib19 0 `shouldBe` 0

    it "should return 1, for fib 1" $ do
      fib19 1 `shouldBe` 1

    it "should return 55, for fib 10" $ do
      fib19 10 `shouldBe` 55

    it "should throw an exception, if used with negative inputs" $ do
      evaluate (fib19 (negate 1)) `shouldThrow` anyException

suite :: [Benchmark]
suite = [
  bgroup "naive" [
    bench "fib 5" $ whnf fib1 5,
    bench "fib 10" $ whnf fib1 10,
    bench "fib 20" $ whnf fib1 20
    ],
  bgroup "de moivre" [
    bench "fib 5" $ whnf fib2 5,
    bench "fib 10" $ whnf fib2 10,
    bench "fib 20" $ whnf fib2 20
    ],
  bgroup "zipWith" [
    bench "fib 5" $ whnf fib3 5,
    bench "fib 10" $ whnf fib3 10,
    bench "fib 20" $ whnf fib3 20
    ],
  bgroup "tailRecursive" [
    bench "fib 5" $ whnf fib4 5,
    bench "fib 10" $ whnf fib4 10,
    bench "fib 20" $ whnf fib4 20
    ],
  bgroup "monadic" [
    bench "fib 5" $ whnf fib5 5,
    bench "fib 10" $ whnf fib5 10,
    bench "fib 20" $ whnf fib5 20
    ],
  bgroup "zipWith2" [
    bench "fib 5" $ whnf fib6 5,
    bench "fib 10" $ whnf fib6 10,
    bench "fib 20" $ whnf fib6 20
    ],
  bgroup "self" [
    bench "fib 5" $ whnf fib7 5,
    bench "fib 10" $ whnf fib7 10,
    bench "fib 20" $ whnf fib7 20
    ],
  bgroup "scanl" [
    bench "fib 5" $ whnf fib8 5,
    bench "fib 10" $ whnf fib8 10,
    bench "fib 20" $ whnf fib8 20
    ],
  bgroup "scanl2" [
    bench "fib 5" $ whnf fib9 5,
    bench "fib 10" $ whnf fib9 10,
    bench "fib 20" $ whnf fib9 20
    ],
  bgroup "fix" [
    bench "fib 5" $ whnf fib10 5,
    bench "fib 10" $ whnf fib10 10,
    bench "fib 20" $ whnf fib10 20
    ],
  bgroup "fix2" [
    bench "fib 5" $ whnf fib11 5,
    bench "fib 10" $ whnf fib11 10,
    bench "fib 20" $ whnf fib11 20
    ],
  bgroup "unfoldr" [
    bench "fib 5" $ whnf fib12 5,
    bench "fib 10" $ whnf fib12 10,
    bench "fib 20" $ whnf fib12 20
    ],
  bgroup "iterate" [
    bench "fib 5" $ whnf fib13 5,
    bench "fib 10" $ whnf fib13 10,
    bench "fib 20" $ whnf fib13 20
    ],
  bgroup "identities" [
    bench "fib 5" $ whnf fib14 5,
    bench "fib 10" $ whnf fib14 10,
    bench "fib 20" $ whnf fib14 20
    ],
  bgroup "matrix" [
    bench "fib 5" $ whnf fib15 5,
    bench "fib 10" $ whnf fib15 10,
    bench "fib 20" $ whnf fib15 20
    ],
  bgroup "fast" [
    bench "fib 5" $ whnf fib16 5,
    bench "fib 10" $ whnf fib16 10,
    bench "fib 20" $ whnf fib16 20
    ],
  bgroup "faster" [
    bench "fib 5" $ whnf fib17 5,
    bench "fib 10" $ whnf fib17 10,
    bench "fib 20" $ whnf fib17 20
    ],
  bgroup "fastest" [
    bench "fib 5" $ whnf fib18 5,
    bench "fib 10" $ whnf fib18 10,
    bench "fib 20" $ whnf fib18 20
    ],
  bgroup "constant" [
    bench "fib 5" $ whnf fib19 5,
    bench "fib 10" $ whnf fib19 10,
    bench "fib 20" $ whnf fib19 20
    ]
  ]

runBench :: IO ()
runBench = defaultMain suite

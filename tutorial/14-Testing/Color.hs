-- Color.hs
module Color where

import Test.QuickCheck

data Color = Red | Green | Blue deriving (Show)

instance Arbitrary Color where
  arbitrary = do
    n <- choose (0, 2) :: Gen Int
    return $ case n of
      0 -> Red
      1 -> Green
      2 -> Blue

colorSamples :: IO [Color]
colorSamples = sample' arbitrary

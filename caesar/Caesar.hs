-- Ceasar.hs
module Caesar where

-- import Data.Text
import Test.Hspec

{-!
newtype PlainText = PlainText Text
newtype EncryptedText = EncryptedText Text
newtype Key = Key Int
!-}

--alphabet :: Text
alphabet :: String
alphabet = ['a' .. 'z']

lookup :: Char -> String -> Int
lookup c a =

--encrypt :: PlainText -> Int -> Text -> EncryptedText
encrypt :: String -> Int -> String -> String
encrypt p k a = "xxx"

--decrypt :: EncryptedText -> Int -> Text -> PlainText
decrypt :: String -> Int -> String -> String
decrypt e k a = "xxx"

test :: IO ()
test = hspec $ do
  describe "encrypt roland 1" $ do
    it "should return xxx" $ do
      encrypt "roland" 1 alphabet `shouldBe` "xxx"

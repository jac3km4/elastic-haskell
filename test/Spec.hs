{-# LANGUAGE OverloadedStrings #-}
import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Tabs" $ do
    it "should be replaced by spaces correctly" $ do
      tabsToSpaces "\ty" 4 `shouldBe` "    y"
      tabsToSpaces "x\ty" 4 `shouldBe` "x   y"
      tabsToSpaces "xxxxxxx\ty" 4 `shouldBe` "xxxxxxx  y"

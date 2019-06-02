module Engine.MovesSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "Something" $ do
    it "does stuffs" $ do
      "stuff" `shouldBe` "stuff"

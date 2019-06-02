module Engine.BoardSpec (spec) where

import Test.Hspec
import Engine.Board

invalidMatrix :: [[String]]
invalidMatrix =[
  ["", "X", ""],
  ["", "", "O"],
  ["", ""]]

validMatrix :: [[String]]
validMatrix = [
  ["", "X", ""],
  ["X", "O", ""],
  ["X", "O", ""]]

validBoard = map convertRow validMatrix
  where convertRow = map convertSquare

spec :: Spec
spec = do
  describe "fromMatrix" $ do
    it "should return nothing for invalid matrices" $ do
      (fromMatrix invalidMatrix) `shouldBe` Nothing
    it "should return a board when given a valid matrix" $ do
      (fromMatrix validMatrix) `shouldBe` Just validBoard

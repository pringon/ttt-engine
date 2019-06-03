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

invalidBoard :: Board
invalidBoard = map convertRow invalidMatrix
  where convertRow = map toSquare

validBoard :: Board
validBoard = map convertRow validMatrix
  where convertRow = map toSquare

spec :: Spec
spec = do
  describe "fromMatrix" $ do
    it "should return nothing for invalid matrices" $ do
      (fromMatrix invalidMatrix) `shouldBe` Nothing
    it "should return a board when given a valid matrix" $ do
      (fromMatrix validMatrix) `shouldBe` Just validBoard
  describe "toMatrix" $ do
    it "should return nothing for invalid boards" $ do
      (toMatrix invalidBoard) `shouldBe` Nothing
    it "should return string matrxi when given a valid board" $ do
      (toMatrix validBoard) `shouldBe` Just validMatrix
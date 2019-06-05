module Engine.BoardSpec (spec) where

import Test.Hspec
import Engine.Board

invalidMatrix :: [[String]]
invalidMatrix = [
  ["", "X", ""],
  ["", "", "O"],
  ["", ""]]

validMatrix :: [[String]]
validMatrix = [
  ["", "X", ""],
  ["X", "O", ""],
  ["X", "O", ""]]

drawMatrix :: [[String]]
drawMatrix = [
  ["O", "X", "X"],
  ["X", "O", "O"],
  ["X", "O", "X"]]

winMatrix :: [[String]]
winMatrix = [
  ["", "X", "O"],
  ["X", "O", "O"],
  ["X", "O", "O"]]

invalidBoard :: Board
invalidBoard = [
  [Empty, X, Empty],
  [Empty, Empty, O],
  [Empty, Empty]] 

validBoard :: Board
validBoard = [
  [Empty, X, Empty],
  [X, O, Empty],
  [X, O, Empty]]

drawBoard :: Board
drawBoard = let (Just b) = fromMatrix drawMatrix in b
winBoard :: Board
winBoard = let (Just b) = fromMatrix winMatrix in b

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
  describe "getState" $ do
    it "should be able to tell that a game is unfinished" $ do
      (getState validBoard) `shouldBe` Unfinished
    it "should be able to tell that a game finished in a draw" $ do
      (getState drawBoard) `shouldBe` Draw
    it "should be able to tell that a game finished in a win" $ do
      (getState winBoard) `shouldBe` Won
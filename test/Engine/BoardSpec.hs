module Engine.BoardSpec (spec) where

import Test.Hspec
import Engine.Board
import Data.Char (toLower)

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

unfinishedMatrix :: [[String]]
unfinishedMatrix = [
  ["O", "O", "X"],
  ["X", "X", "O"],
  ["O", "X", ""]]

lowercaseMatrix :: [[String]]
lowercaseMatrix = map (map (map toLower)) validMatrix

drawMatrix :: [[String]]
drawMatrix = [
  ["O", "X", "X"],
  ["X", "O", "O"],
  ["X", "O", "X"]]

colMatrix :: [[String]]
colMatrix = [
  ["", "X", "O"],
  ["X", "O", "O"],
  ["X", "O", "O"]]

rowMatrix :: [[String]]
rowMatrix = [
  ["O", "O", "O"],
  ["", "", ""],
  ["", "", ""]]

mainDiagMatrix :: [[String]]
mainDiagMatrix = [
  ["O", "", ""],
  ["", "O", ""],
  ["", "", "O"]]

secondDiagMatrix :: [[String]]
secondDiagMatrix = [
  ["", "", "O"],
  ["", "O", ""],
  ["O", "", ""]]

colPlayerMatrix :: [[String]]
colPlayerMatrix = [
  ["", "", "X"],
  ["", "", "X"],
  ["", "", "X"]]

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

unfinishedBoard :: Board
unfinishedBoard = let (Just b) = fromMatrix unfinishedMatrix in b
drawBoard :: Board
drawBoard = let (Just b) = fromMatrix drawMatrix in b
rowBoard :: Board
rowBoard = let (Just b) = fromMatrix rowMatrix in b
colBoard :: Board
colBoard = let (Just b) = fromMatrix colMatrix in b
mainDiagBoard :: Board
mainDiagBoard = let (Just b) = fromMatrix colMatrix in b
secondDiagBoard :: Board
secondDiagBoard = let (Just b) = fromMatrix colMatrix in b
colPlayerBoard :: Board
colPlayerBoard = let (Just b) = fromMatrix colPlayerMatrix in b

spec :: Spec
spec = do
  describe "fromMatrix" $ do
    it "should return nothing for invalid matrices" $ do
      (fromMatrix invalidMatrix) `shouldBe` Nothing
    it "should return a board when given a valid matrix" $ do
      (fromMatrix validMatrix) `shouldBe` Just validBoard
    it "should return a board when given a valid, lowercase matrix" $ do
      (fromMatrix lowercaseMatrix) `shouldBe` Just validBoard
  describe "toMatrix" $ do
    it "should return nothing for invalid boards" $ do
      (toMatrix invalidBoard) `shouldBe` Nothing
    it "should return string matrxi when given a valid board" $ do
      (toMatrix validBoard) `shouldBe` Just validMatrix
  describe "getState" $ do
    it "should be able to tell that a game is unfinished" $ do
      (getState unfinishedBoard) `shouldBe` (Unfinished, Empty)
      (getState validBoard) `shouldBe` (Unfinished, Empty)
    it "should be able to tell that a game finished in a draw" $ do
      (getState drawBoard) `shouldBe` (Draw, Empty)
    it "should be able to see winning rows" $ do
      (getState rowBoard) `shouldBe` (Won, O)
    it "should be able to see winning columns" $ do
      (getState colBoard) `shouldBe` (Won, O)
    it "should be able to see win in main diagonal" $ do
      (getState mainDiagBoard) `shouldBe` (Won, O)
    it "should be able to see win in secondary diagonal" $ do
      (getState secondDiagBoard) `shouldBe` (Won, O)
    it "should be able to see a player winning by column" $ do
      (getState colPlayerBoard) `shouldBe` (Won, X)
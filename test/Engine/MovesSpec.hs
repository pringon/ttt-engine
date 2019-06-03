module Engine.MovesSpec (spec) where

import Test.Hspec
import Engine.Board
import Engine.Moves

stubMatrix :: [[String]]
stubMatrix = [
  ["", "X", ""],
  ["X", "O", ""],
  ["X", "O", ""]]

moveTakenMatrix :: [[String]]
moveTakenMatrix = [
  ["O", "X", ""],
  ["X", "O", ""],
  ["X", "O", ""]]

stubBoard :: Board
stubBoard =
  let (Just board) = fromMatrix stubMatrix
  in board

moveTakenBoard :: Board
moveTakenBoard =
  let (Just board) = fromMatrix moveTakenMatrix
  in board

moves :: [Move]
moves = [(0, 0), (0, 2), (1, 2), (2, 2)]

spec :: Spec
spec = do
  describe "getMoves" $ do
    it "should return all empty squares on board" $ do
      (getMoves stubBoard) `shouldBe` moves
  describe "takeMove" $ do
    it "should return the correct board state when given a valid board state and a valid move" $ do
      (takeMove stubBoard (0, 0) Engine) `shouldBe` moveTakenBoard
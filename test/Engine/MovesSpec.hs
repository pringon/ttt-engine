module Engine.MovesSpec (spec) where

import Test.Hspec
import Engine.Board
import Engine.Moves

stubMatrix :: [[String]]
stubMatrix = [
  ["", "X", ""],
  ["X", "O", ""],
  ["X", "O", ""]]

stubBoard :: Board
stubBoard =
  let (Just board) = fromMatrix stubMatrix
  in board

moves :: [Move]
moves = [(0, 0), (0, 2), (1, 2), (2, 2)]

spec :: Spec
spec = do
  describe "getMoves" $ do
    it "should return all empty squares on board" $ do
      (getMoves stubBoard) `shouldBe` moves

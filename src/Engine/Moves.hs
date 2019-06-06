module Engine.Moves
  (findBestMove
  ,getMoves
  ,takeMove
  ,Move
  ,Color(..)
  ) where

import Data.List
import Engine.Board

type Move = (Int, Int)
type Score = Int
data Color = Engine | Player deriving (Eq)

findBestMove :: Color -> Board -> (Move, Score)
findBestMove c b = 
  case getState b of
    Draw -> ((99, 99), 0)
    Won -> ((99, 99), if c == Engine then 100 else -100)
    Unfinished -> inverse . foldl (\acc x -> if snd x > snd acc then x else acc) ((99, 99), minBound :: Int) . mapMoves b c $ getMoves b
  where inverse (move, score) = (move, -score)
        -- compareMoves c (_, x) (_, y) = if c == Engine then y `compare` x else x `compare` y
        -- getBound c = if c == Engine then minBound else maxBound

getMoves :: Board -> [Move]
getMoves b = concat $ zipWith parseRow b [0..]

takeMove :: Board -> Color -> Move -> Board
takeMove b c m = zipWith (updateRow m c) b [0..]

mapMoves :: Board -> Color -> [Move] -> [(Move, Score)]
mapMoves b c moves = zipWith (\(_, score) move -> (move, score)) (getMoveScores b c moves) moves
  where getMoveScores b c moves = map (findBestMove (toggleColor c) . takeMove b c) moves

updateRow :: Move -> Color -> [Square] -> Int -> [Square]
updateRow m c r i = zipWith (\x j -> if (i, j) == m then getSymbol c else x) r [0..]

getSymbol :: Color -> Square
getSymbol Engine = O
getSymbol Player = X

toggleColor :: Color -> Color
toggleColor Engine = Player
toggleColor Player = Engine

parseRow :: [Square] -> Int -> [Move]
parseRow r i = map (\(Just x) -> x) $ filter (/= Nothing) $ zipWith (\x j -> if x == Empty then Just (i, j) else Nothing) r [0..]
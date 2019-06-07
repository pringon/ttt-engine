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
    Unfinished -> 
      let possibleMoves = mapMoves b c $ getMoves b
          minScore = ((99, 99), minBound :: Int)
      in inverse . foldl maxScore minScore $ possibleMoves
  where inverse (move, score) = (move, -score)
        maxScore acc x = if snd x > snd acc then x else acc

mapMoves :: Board -> Color -> [Move] -> [(Move, Score)]
mapMoves b c moves = zipWith zipScoreMove  (getMoveScores b c moves) moves
  where zipScoreMove (_, score) move = (move, score)
        getMoveScores b c moves = map (findBestMove (toggleColor c) . takeMove b c) moves

getMoves :: Board -> [Move]
getMoves b = concat $ zipWith parseRow b [0..]

takeMove :: Board -> Color -> Move -> Board
takeMove b c m = zipWith (updateRow m c) b [0..]

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
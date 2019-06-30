module Engine.Moves
  (findBestMove
  ,getMoves
  ,takeMove
  ,Move
  ,Color(..)
  ) where

import Data.List
import Data.Maybe
import Engine.Board

type Move = (Int, Int)
type Score = Int
type Depth = Int
data Color = Engine | Player deriving (Eq, Show)

findBestMove :: Color -> Depth -> Board -> (Move, Score)
findBestMove c d b = 
  case getState b of
    (Draw, _) -> ((99, 99), 0)
    (Won, s) -> ((99, 99), -100 + d)
    (Unfinished, _) -> 
      let possibleMoves = mapMoves b c d $ getMoves b
          minScore = ((99, 99), minBound :: Int)
          bestMove = foldl maxScore minScore $ possibleMoves
      in bestMove
  where maxScore acc x = if snd x > snd acc then x else acc

mapMoves :: Board -> Color -> Depth -> [Move] -> [(Move, Score)]
mapMoves b c d moves = zipWith zipScoreMove (getMoveScores b c d moves) moves
  where zipScoreMove (_, score) move = (move, score)
        inverse (move, score) = (move, -score)
        getMoveScores b c d moves = map (inverse . findBestMove (toggleColor c) (d + 1) . takeMove b c) moves

getMoves :: Board -> [Move]
getMoves b = concat $ zipWith parseRow b [0..]

takeMove :: Board -> Color -> Move -> Board
takeMove b c m = zipWith (updateRow m c) b [0..]

updateRow :: Move -> Color -> [Square] -> Int -> [Square]
updateRow (i', j') c r i = 
  if i' == i then
    zipWith (\x j -> if j == j' then getSymbol c else x) r [0..]
  else
    r

getSymbol :: Color -> Square
getSymbol Engine = O
getSymbol Player = X

toggleColor :: Color -> Color
toggleColor Engine = Player
toggleColor Player = Engine

parseRow :: [Square] -> Int -> [Move]
parseRow r i = map fromJust $ filter (/= Nothing) $ zipWith (\x j -> if x == Empty then Just (i, j) else Nothing) r [0..]
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
data Color = Engine | Player

findBestMove :: Board -> Color -> (Move, Score)
findBestMove b c = ((99, 99), 0)

getMoves :: Board -> [Move]
getMoves b = concat $ zipWith parseRow b [0..]

takeMove :: Board -> Move -> Color -> Board
takeMove b m c = zipWith (updateRow m c) b [0..]

updateRow :: Move -> Color -> [Square] -> Int -> [Square]
updateRow m c r i = zipWith (\x j -> if (i, j) == m then getSymbol c else x) r [0..]

getSymbol :: Color -> Square
getSymbol Engine = O
getSymbol Player = X

parseRow :: [Square] -> Int -> [Move]
parseRow r i = map (\(Just x) -> x) $ filter (/= Nothing) $ zipWith (\x j -> if x == Empty then Just (i, j) else Nothing) r [0..]
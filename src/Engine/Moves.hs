module Engine.Moves
    (getMoves
    ,takeMove
    ,Move
    ,Color(..)
    ) where

import Data.List
import Engine.Board

type Move = (Int, Int)
data Color = Engine | Player

getMoves :: Board -> [Move]
getMoves b = concat $ zipWith parseRows b [0..]

takeMove :: Board -> Move -> Color -> Board
takeMove b m c = []

parseRows :: [Square] -> Int -> [Move]
parseRows r i = map (\(Just x) -> x) $ filter (/= Nothing) $ zipWith (\x j -> if x == Empty then Just (i, j) else Nothing) r [0..]
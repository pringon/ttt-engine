module Engine.Moves
    (getMoves
    ,Move
    ) where

import Data.List
import Engine.Board

type Move = (Int, Int)

getMoves :: Board -> [Move]
getMoves b = concat $ zipWith parseRows b [0..]

parseRows :: [Square] -> Int -> [Move]
parseRows r i = map (\(Just x) -> x) $ filter (/= Nothing) $ zipWith (\x j -> if x == Empty then Just (i, j) else Nothing) r [0..]
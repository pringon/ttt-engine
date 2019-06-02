module Engine.Moves
    (getMoves
    ,Move
    ) where

import Engine.Board

type Move = (Int, Int)

getMoves :: Board -> [Move]
getMoves b = []

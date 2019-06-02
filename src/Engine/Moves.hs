module Engine.Moves
    ( 
    ) where

data Square = X | O | Empty deriving (Show, Read, Eq)
newtype Board = Board { getBoard :: [[Square]] }

-- getMoves :: Board -> [Board]
-- getMoves (Board b) = 

-- getEmptySquares :: Board -> [(Int, Int)]
-- getEmptySquares = foldl (\acc x -> acc ++ (findEmpty x))
--  where findEmpty = findIndices (== Empty)

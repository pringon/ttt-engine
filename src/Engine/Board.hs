module Engine.Board
  (fromMatrix
  ,convertSquare
  ,Board
  ) where

data Square = X | O | Empty deriving (Show, Read, Eq)
type Board = [[Square]]

fromMatrix :: [[String]] -> Maybe Board
fromMatrix sboard =
  if isValid sboard then
    Just (map fromRow sboard)
  else
    Nothing

fromRow :: [String] -> [Square]
fromRow = map convertSquare

convertSquare :: String -> Square
convertSquare square = 
  case reads square of
    [(X, "")] -> X
    [(O, "")] -> O
    _ -> Empty


isValid :: [[a]] -> Bool
isValid board =
  let len = length board
  in all (\x -> length x == len) board

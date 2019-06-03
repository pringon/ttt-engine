module Engine.Board
  (fromMatrix
  ,toMatrix
  ,toSquare
  ,Square(..)
  ,Board
  ) where

data Square = X | O | Empty deriving (Show, Read, Eq)
type Board = [[Square]]

fromMatrix :: [[String]] -> Maybe Board
fromMatrix sboard =
  if isValid sboard then
    Just (map (mapRow toSquare) sboard)
  else
    Nothing

toMatrix :: Board -> Maybe [[String]]
toMatrix board = 
  if isValid board then
    Just (map (mapRow fromSquare) board)
  else
    Nothing

mapRow :: (a -> b) -> [a] -> [b]
mapRow f = map f

toSquare :: String -> Square
toSquare square = 
  case reads square of
    [(X, "")] -> X
    [(O, "")] -> O
    _ -> Empty

fromSquare :: Square -> String
fromSquare square = 
  case square of
    X -> "X"
    O -> "O"
    _ -> ""


isValid :: [[a]] -> Bool
isValid board =
  let len = length board
  in all (\x -> length x == len) board

module Engine.Board
  (fromMatrix
  ,toMatrix
  ,getState
  ,toSquare
  ,Square(..)
  ,State(..)
  ,Board
  ) where

data Square = X | O | Empty deriving (Show, Read, Eq)
data State = Unfinished | Draw | Loss | Win deriving (Show, Eq)
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

getState :: Board -> State
getState b = Unfinished

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

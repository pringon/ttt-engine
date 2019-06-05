module Engine.Board
  (fromMatrix
  ,toMatrix
  ,getState
  ,Square(..)
  ,State(..)
  ,Board
  ) where

import Control.Applicative

data Square = X | O | Empty deriving (Show, Read, Eq)
type Board = [[Square]]

data State = Unfinished | Draw | Won deriving (Show, Eq)
instance Ord State where
  Won `compare` Won = EQ
  Unfinished `compare` Unfinished = EQ
  Draw `compare` Draw = EQ
  Won `compare` _ = GT
  _ `compare` Won = LT
  Draw `compare` _ = LT
  _ `compare` Draw = GT

fromMatrix :: [[String]] -> Maybe Board
fromMatrix sboard =
  if isValid sboard then
    Just (map (map toSquare) sboard)
  else
    Nothing

toMatrix :: Board -> Maybe [[String]]
toMatrix board = 
  if isValid board then
    Just (map (map fromSquare) board)
  else
    Nothing

getState :: Board -> State
getState b =
  let r = foldl (\acc x -> max acc x) Draw $ map checkLine b
      b' = rowToCol b
      c = foldl (\acc x -> max acc x) Draw $ map checkLine b'
      d = checkDiags b
  in max d $ max r c

checkLine :: [Square] -> State
checkLine b = Unfinished

rowToCol :: [[a]] -> [[a]]
rowToCol b =
  let accum = ZipList (replicate (length b) []) 
  in getZipList $ foldr (\xs acc -> (:) <$> (ZipList xs) <*> acc) accum b

checkDiags :: Board -> State
checkDiags b = Unfinished

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

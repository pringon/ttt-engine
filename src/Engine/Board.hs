module Engine.Board
  (fromMatrix
  ,toMatrix
  ,getState
  ,Square(..)
  ,State(..)
  ,Board
  ) where

import Control.Applicative
import Data.Char (toUpper)

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
    Just (map (map $ toSquare . map toUpper) sboard)
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

checkDiags :: Board -> State
checkDiags b = 
  let len = length b
      d1 = checkLine $ zipWith (\x y -> x !! y) b [0..]
      d2 = checkLine $ zipWith (\x y -> x !! y) b [len-1,len-2..]
  in max d1 d2

checkLine :: [Square] -> State
checkLine l = foldl (checkSquare (l !! 0)) Won l
  where checkSquare first s Empty = min s Unfinished
        checkSquare first s x
          | x == first = min s Won
          | otherwise = if s == Unfinished then Unfinished else Draw

rowToCol :: [[a]] -> [[a]]
rowToCol b =
  let accum = ZipList (replicate (length b) []) 
  in getZipList $ foldr (\xs acc -> (:) <$> (ZipList xs) <*> acc) accum b

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

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

getState :: Board -> (State, Square)
getState b =
  let r = foldl (\acc x -> bestState acc x) (Draw, Empty) $ map checkLine b
      b' = rowToCol b
      c = foldl (\acc x -> bestState acc x) (Draw, Empty) $ map checkLine b'
      d = checkDiags b
  in bestState d $ bestState r c

bestState :: (State, Square) -> (State, Square) -> (State, Square)
bestState (st1, sq1) (st2, sq2)
  | st1 > st2 = (st1, sq1)
  | otherwise = (st2, sq2)

worstState :: (State, Square) -> (State, Square) -> (State, Square)
worstState (st1, sq1) (st2, sq2)
  | st1 < st2 = (st1, sq1)
  | otherwise = (st2, sq2)

checkDiags :: Board -> (State, Square)
checkDiags b = 
  let len = length b
      d1 = checkLine $ zipWith (\x y -> x !! y) b [0..]
      d2 = checkLine $ zipWith (\x y -> x !! y) b [len-1,len-2..]
  in bestState d1 d2

checkLine :: [Square] -> (State, Square)
checkLine l = foldl (checkSquare (l !! 0)) (Won, l !! 0) l 
  where checkSquare :: Square -> (State, Square) -> Square -> (State, Square)
        checkSquare first s Empty = worstState s (Unfinished, Empty)
        checkSquare first s x
          | x == first = worstState s (Won, first)
          | otherwise = 
            let (st, _) = s
                newSt = if st == Unfinished then Unfinished else Draw
            in (newSt, Empty)

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

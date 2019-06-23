{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (toLower)
import Control.Monad

import Network.Wai
import Network.HTTP.Types (status200, status400, Header)
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString.Char8 as BS8

import Engine.Moves
import Engine.Board

defaultHeaders :: [Header]
defaultHeaders = [
  ("Content-Type", "text/plain"),
  ("Access-Control-Allow-Origin", "*")]

parseBody :: String -> Maybe Board
parseBody rawBody =
  case reads rawBody of
    [(b, "")] -> fromMatrix b
    _ -> Nothing

responseBody :: Maybe Square -> Maybe Move -> Bool -> String
responseBody winner move draw =
  case winner of
    (Just square) -> withWinner ("\"" ++ show square ++ "\"") move draw
    (Nothing) -> withWinner "null" move draw
  where
    withWinner :: String -> Maybe Move -> Bool -> String
    withWinner winner (Just (x, y)) draw = makeString winner (show [x, y]) draw
    withWinner winner Nothing draw = makeString winner "null" draw
    makeString :: String -> String -> Bool -> String
    makeString winner move draw = "{ \"winner\": " ++ winner ++ ", \"move\": " ++ move ++", \"draw\": " ++ boolToJson draw ++ " }"
    boolToJson :: Bool -> String
    boolToJson b = Prelude.map toLower $ show b

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application req res = do
  body <- requestBody $ req
  let board = parseBody $ BS8.unpack body
  case board of 
    (Just b) -> respond b res
    _ -> handleError res

main :: IO ()
main = run 3000 application

respond :: Board -> (Response -> IO ResponseReceived) -> IO ResponseReceived
respond b res = do
  let (move, _) = findBestMove Engine 0 b
  let body = case getState b of
        (Won, s) -> responseBody (Just s) Nothing False
        (Draw, _) -> responseBody Nothing Nothing True
        _ ->
          case getState (takeMove b Engine move) of
            (Won, s) -> responseBody (Just s) (Just move) False
            (Draw, _) -> responseBody Nothing (Just move) True
            _ -> responseBody Nothing (Just move) False
  res $ responseLBS status200 defaultHeaders $ BL8.pack body

handleError :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleError res = res $ responseLBS status400 defaultHeaders "Incorrect board format."

{-# LANGUAGE OverloadedStrings #-}
module Main where

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

extractMove :: (Move, a) -> [Int]
extractMove ((x, y), _) = [x, y]

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
  let nextMove = findBestMove Engine 0 b
  res $ responseLBS status200 defaultHeaders $ BL8.pack . show . extractMove $ nextMove

handleError :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleError res = res $ responseLBS status400 defaultHeaders "Incorrect board format."


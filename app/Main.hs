{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as BS

import Engine.Moves
import Engine.Board

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
  let board = parseBody $ BS.unpack body
  case board of 
    (Just b) -> respond b res
    _ -> handleError res

main :: IO ()
main = run 3000 application

respond :: Board -> (Response -> IO ResponseReceived) -> IO ResponseReceived
respond b res = do
  let nextMove = findBestMove Engine 0 b
  res $ responseLBS status200 [("Content-Type", "text/plain")] $ BL.fromChunks . return . BS.pack . show . extractMove $ nextMove

handleError :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleError res = res $ 
  responseLBS status400 [("Content-Type", "text/plain")] "Incorrect board format."
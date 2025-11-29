{-# LANGUAGE OverloadedStrings #-}

module Server.SSE where

import Control.Concurrent.STM
import Control.Exception
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types
import Network.Wai

type EventChannel = TChan (Maybe BL.ByteString)

type BroadcastChannel = TVar [EventChannel]

createBroadcastChannel :: IO BroadcastChannel
createBroadcastChannel = newTVarIO []

createEventChannel :: BroadcastChannel -> IO EventChannel
createEventChannel broadcast = do
  chan <- newTChanIO
  atomically $ modifyTVar broadcast (chan :)
  return chan

removeEventChannel :: BroadcastChannel -> EventChannel -> IO ()
removeEventChannel broadcast chan = atomically $ do
  chans <- readTVar broadcast
  writeTVar broadcast $ filter (/= chan) chans

broadcastEvent :: BroadcastChannel -> BL.ByteString -> IO ()
broadcastEvent broadcast data' = atomically $ do
  chans <- readTVar broadcast
  mapM_ (\chan -> writeTChan chan (Just data')) chans

sseApplication :: BroadcastChannel -> Application
sseApplication broadcast _ respond = do
  chan <- createEventChannel broadcast

  let headers =
        [ ("Content-Type", "text/event-stream"),
          ("Cache-Control", "no-cache"),
          ("Connection", "keep-alive")
        ]

  respond $ responseStream status200 headers $ \write flush -> do
    let sendEvent' event = do
          write $ byteString $ BS.pack "data: "
          write $ lazyByteString event
          write $ byteString $ BS.pack "\n\n"
          flush

    sendEvent' (BL.fromStrict $ BS.pack "connected")

    let loop = do
          event <- atomically $ readTChan chan
          case event of
            Nothing -> return ()
            Just data' -> do
              sendEvent' data'
              loop

    loop `finally` removeEventChannel broadcast chan

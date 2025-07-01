{-# LANGUAGE OverloadedStrings #-}

module Web.Minion.Examples.ServerEvent (app, api) where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Data.Binary.Builder as Binary
import Data.List.NonEmpty qualified as NonEmpty
import Web.Minion
import Web.Minion.Response.ServerEvent

{- FOURMOLU_DISABLE -}
-- The server accepts strings from the console and sends them to clients via SSE.
-- $ cabal run minion-wai-extra-sse-example -v0 |
--                                              | $ curl localhost:9001/api/sse
-- hello                                        |
--                                              | event:typed_string
--                                              | data:hello
-- how are you                                  |
--                                              | event:typed_string
--                                              | data:how are you
-- ^C                                           |
--                                              | curl: (18) transfer closed with outstanding read data remaining
{- FOURMOLU_ENABLE -}
app :: IO (ApplicationM IO)
app = do
  chan <- newChan @String
  _ <- forkIO $ forever do
    getLine >>= writeChan chan
  pure $ \req resp -> runReaderT (serve api req resp) chan

api :: Router Void (ReaderT (Chan String) IO)
api = "api" /> "sse" /> handle GET sse

sse :: ReaderT (Chan String) IO (EventSource ServerEvent)
sse = do
  chan <- ask
  pure $ EventSource
    do
      NonEmpty.singleton
        . ServerEvent (Just $ Binary.putStringUtf8 "typed_string") Nothing
        . pure
        . Binary.putStringUtf8
        <$> readChan chan
    do \_ -> putStrLn "Client has been disconnected"

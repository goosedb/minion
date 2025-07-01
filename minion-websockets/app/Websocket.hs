module Main where

import Network.Wai.Handler.Warp qualified as Warp
import Web.Minion.Examples.Websocket qualified

main :: IO ()
main = Warp.run 9001 Web.Minion.Examples.Websocket.app

module Main where

import Network.Wai.Handler.Warp qualified as Warp
import Web.Minion.Examples.Json

main :: IO ()
main = Warp.run 9001 Web.Minion.Examples.Json.app

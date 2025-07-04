module Main where

import Network.Wai.Handler.Warp qualified as Warp
import Web.Minion.Examples.Static qualified

main :: IO ()
main = Warp.run 9001 Web.Minion.Examples.Static.app

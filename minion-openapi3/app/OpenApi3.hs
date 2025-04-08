module Main where

import Network.Wai.Handler.Warp qualified as Warp
import Web.Minion.Examples.OpenApi3

main :: IO ()
main = Warp.run 9001 app

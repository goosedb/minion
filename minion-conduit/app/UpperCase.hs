module Main where

import Network.Wai.Handler.Warp qualified as Warp
import Web.Minion.Examples.Conduit.UpperCase

main :: IO ()
main = Warp.run 9001 Web.Minion.Examples.Conduit.UpperCase.app

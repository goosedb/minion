module Main where

import Network.Wai.Handler.Warp qualified as Warp
import Web.Minion.Examples.Jwt qualified

main :: IO ()
main = do
  app <- Web.Minion.Examples.Jwt.app
  Warp.run 9001 app

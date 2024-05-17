module Main where

import Network.Wai.Handler.Warp qualified as Warp
import Web.Minion.Examples.BasicAuth qualified

main :: IO ()
main = do
  app <- Web.Minion.Examples.BasicAuth.app
  Warp.run 9001 app

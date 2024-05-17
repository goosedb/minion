module Main where

import Network.Wai.Handler.Warp qualified as Warp
import Web.Minion.Examples.Multipart qualified

main :: IO ()
main = do
  app <- Web.Minion.Examples.Multipart.app
  Warp.run 9001 app

module Main where

import Network.Wai.Handler.Warp qualified as Warp

import Web.Minion.Examples.ComplexResponse qualified

main :: IO ()
main = do
  Warp.run 9001 Web.Minion.Examples.ComplexResponse.app

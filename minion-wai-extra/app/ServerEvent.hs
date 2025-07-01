{-# LANGUAGE TemplateHaskell #-}

module Main where

import Network.Wai.Handler.Warp qualified as Warp
import Web.Minion.Examples.ServerEvent qualified

main :: IO ()
main = do
  app <- Web.Minion.Examples.ServerEvent.app
  Warp.run 9002 app
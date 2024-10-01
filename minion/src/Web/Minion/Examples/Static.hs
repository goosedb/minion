module Web.Minion.Examples.Static (app) where

import Web.Minion
import Web.Minion.Static

app :: ApplicationM IO
app = serve api

api :: Router Void IO
api = "api" /> "static" /> staticFiles defaultExtsMap files
 where
  files =
    [ ("folder/data.json", "{ \"key\": 1 }")
    , ("another/folder/data.csv", "a;b;c\n1;2;3")
    ]

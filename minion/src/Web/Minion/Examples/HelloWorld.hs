{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Minion.Examples.HelloWorld (app) where

import Web.Minion

app :: ApplicationM IO
app = serve @IO api

api :: Router Void IO
api =
  "api"
    /> [ "about" /> handlePlainText @String GET (pure "Hello-World Minion server")
       , "hello" /> capture @String "name" .> handlePlainText @String GET (\name -> pure $ "Hello, " <> name <> "!")
       ]

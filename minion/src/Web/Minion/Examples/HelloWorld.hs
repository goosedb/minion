module Web.Minion.Examples.HelloWorld (app) where

import Web.Minion

app :: ApplicationM IO
app = serve api

api :: Router Void IO
api =
  "api"
    /> [ "about" /> handleBody @Ok @'[PlainText] @String GET about
       , "hello" /> capture @String "name" .> handleBody @Ok @'[PlainText] @String GET hello
       ]
 where
  about = pure "Hello-World Minion server"
  hello name = pure $ "Hello, " <> name <> "!"

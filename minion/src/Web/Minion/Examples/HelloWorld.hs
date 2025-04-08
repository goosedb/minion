module Web.Minion.Examples.HelloWorld (app) where

import Web.Minion

app :: ApplicationM IO
app = serve api

api :: Router Void IO
api =
  "api"
    /> [ "about" /> handlePlainText @String GET about
       , "hello" /> capture @String "name" .> handlePlainText @String GET hello
       ]
 where
  about = pure "Hello-World Minion server"
  hello name = pure $ "Hello, " <> name <> "!"

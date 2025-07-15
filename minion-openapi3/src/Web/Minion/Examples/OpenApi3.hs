module Web.Minion.Examples.OpenApi3 (app) where

import Web.Minion
import Web.Minion.OpenApi3
import Web.Minion.OpenApi3.Ui

app :: ApplicationM IO
app = serve api

api :: Router Void IO
api =
  [ hideIntrospection publicApi
  , "openapi" /> openapi3 publicApi
  ]
 where

publicApi :: Router' '[OpenApi3] Void IO
publicApi =
  "api/public"
    /> description (TagText "public")
    /> "hello"
    /> description (DescriptionText "Greets user")
    /> queryParam @Required @String "name"
    .> handleBody @Ok @'[PlainText] @String GET (\name -> pure $ "Hello, " <> name <> "!")

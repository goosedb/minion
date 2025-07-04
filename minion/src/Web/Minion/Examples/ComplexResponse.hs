module Web.Minion.Examples.ComplexResponse (app) where

import Data.Unique (hashUnique, newUnique)
import Web.Minion
import Web.Minion.Response.Body (Redirect (..))
import Web.Minion.Response.Union

app :: ApplicationM IO
app = serve api

api :: Router Void IO
api = "api" /> "complex" /> handle GET endpoint

endpoint ::
  IO
    ( Union
        [ RespBody Ok '[Json] Int
        , Redirect
        ]
    )
endpoint = do
  a <- (== 0) . (`mod` 2) . hashUnique <$> newUnique
  let respJson = RespBody @Ok @'[Json] @Int 1
      respRedirect = Redirect "https://google.com"
  pure if a then inject respJson else inject respRedirect

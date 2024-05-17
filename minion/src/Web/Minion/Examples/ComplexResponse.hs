module Web.Minion.Examples.ComplexResponse (app) where

import Data.Unique (hashUnique, newUnique)
import Web.Minion
import Web.Minion.Json (Json)
import Web.Minion.Response.Header
import Web.Minion.Response.Status
import Web.Minion.Response.Union

app :: ApplicationM IO
app = serve api

api :: Router Void IO
api = "api" /> "complex" /> handle GET endpoint

endpoint ::
  IO
    ( Union
        [ RespBody '[Json] Int
        , WithStatus SeeOther (AddHeaders '[AddHeader "Location" RawHeaderValue] NoBody)
        ]
    )
endpoint = do
  a <- (== 0) . (`mod` 2) . hashUnique <$> newUnique
  let respJson = RespBody @'[Json] @Int 1
      redirectHeader = AddHeader @"Location" (RawHeaderValue "https://google.com")
      respSeeOther = WithStatus @SeeOther (AddHeaders (redirectHeader :# HNil) NoBody)
  pure if a then inject respJson else inject respSeeOther

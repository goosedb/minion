module Web.Minion.Response.Body.Json where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON)
import Network.HTTP.Types qualified as Http
import Web.Minion.Args (DelayedArgs, HandleArgs, type (~>))
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.Json (Json)
import Web.Minion.Response.Body (RespBody, handleBody)
import Web.Minion.Router (
  Router',
 )

{-# INLINE handleJson #-}
handleJson ::
  forall o m ts i st.
  (HandleArgs ts st m) =>
  (ToJSON o) =>
  (MonadIO m) =>
  (I.Introspection i I.Response (RespBody '[Json] o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handleJson = handleBody @'[Json] @o @m @ts

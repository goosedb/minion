module Web.Minion.Response.Body.PlainText where

import Network.HTTP.Types qualified as Http
import Web.Minion.Args (DelayedArgs, HandleArgs, type (~>))
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.PlainText
import Web.Minion.Response (ToResponse)
import Web.Minion.Response.Body (RespBody, handleBody)
import Web.Minion.Router (
  Router',
 )

{-# INLINE handlePlainText #-}
handlePlainText ::
  forall o m ts i st.
  (HandleArgs ts st m) =>
  (ToResponse m (RespBody '[PlainText] o)) =>
  (I.Introspection i I.Response (RespBody '[PlainText] o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handlePlainText = handleBody @'[PlainText] @o @m @ts

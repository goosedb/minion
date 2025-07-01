module Web.Minion.Response.Body.PlainText where

import Network.HTTP.Types qualified as Http
import Web.Minion.Args (DelayedArgs, HandleArgs, type (~>))
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.PlainText
import Web.Minion.Response (ToResponse)
import Web.Minion.Response.Body (RespBody, handleBody)
import Web.Minion.Response.Status (OK)
import Web.Minion.Router (
  Router',
 )

{-# INLINE handlePlainText #-}
handlePlainText ::
  forall o m ts i st.
  (HandleArgs ts st m) =>
  (ToResponse m (RespBody OK '[PlainText] o)) =>
  (I.Introspection i I.Response (RespBody OK '[PlainText] o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handlePlainText = handleBody @OK @'[PlainText] @o @m @ts

{-# INLINE handlePlainTextStatus #-}
handlePlainTextStatus ::
  forall status o m ts i st.
  (HandleArgs ts st m) =>
  (ToResponse m (RespBody status '[PlainText] o)) =>
  (I.Introspection i I.Response (RespBody status '[PlainText] o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handlePlainTextStatus = handleBody @status @'[PlainText] @o @m @ts

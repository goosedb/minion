module Web.Minion.Response.Body.OctetStream where

import Network.HTTP.Types qualified as Http
import Web.Minion.Args (DelayedArgs, HandleArgs, type (~>))
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.OctetStream
import Web.Minion.Response (ToResponse)
import Web.Minion.Response.Body (RespBody, RespBodyStream, handleBody, handleBodyStream)
import Web.Minion.Response.Status (OK)
import Web.Minion.Router (Router')

{-# INLINE handleOctet #-}
handleOctet ::
  forall o m ts i st.
  (HandleArgs ts st m) =>
  (ToResponse m (RespBody OK '[OctetStream Bytes] o)) =>
  (I.Introspection i I.Response (RespBody OK '[OctetStream Bytes] o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handleOctet = handleBody @OK @'[OctetStream Bytes] @o @m @ts

{-# INLINE handleOctetStatus #-}
handleOctetStatus ::
  forall status o m ts i st.
  (HandleArgs ts st m) =>
  (ToResponse m (RespBody status '[OctetStream Bytes] o)) =>
  (I.Introspection i I.Response (RespBody status '[OctetStream Bytes] o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handleOctetStatus = handleBody @status @'[OctetStream Bytes] @o @m @ts

{-# INLINE handleOctetStream #-}
handleOctetStream ::
  forall o m ts i st.
  (HandleArgs ts st m) =>
  (ToResponse m (RespBodyStream OK '[OctetStream Chunks] o)) =>
  (I.Introspection i I.Response (RespBodyStream OK '[OctetStream Chunks] o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handleOctetStream = handleBodyStream @OK @'[OctetStream Chunks] @o @m @ts

{-# INLINE handleOctetStreamStatus #-}
handleOctetStreamStatus ::
  forall status o m ts i st.
  (HandleArgs ts st m) =>
  (ToResponse m (RespBodyStream status '[OctetStream Chunks] o)) =>
  (I.Introspection i I.Response (RespBodyStream status '[OctetStream Chunks] o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handleOctetStreamStatus = handleBodyStream @status @'[OctetStream Chunks] @o @m @ts

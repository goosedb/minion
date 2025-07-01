module Web.Minion.Request.Body.Raw where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Web.Minion.Args
import Web.Minion.Codec.Decode (Decode, DecodeStream)
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.OctetStream (Bytes, OctetStream)
import Web.Minion.Request.Body (ReqBody (..), ReqBodyStream, reqBody, reqBodyStream)
import Web.Minion.Router

lazyBytesBody ::
  forall r m i ts.
  (I.Introspection i I.Request (ReqBody '[OctetStream Bytes] r)) =>
  (MonadIO m, MonadThrow m, Decode (OctetStream Bytes) r) =>
  -- | .
  ValueCombinator i (WithReq m (ReqBody '[OctetStream Bytes] r)) ts m
lazyBytesBody = reqBody

chunksBody ::
  forall r m i ts.
  (I.Introspection i I.Request (ReqBodyStream '[OctetStream Bytes] r)) =>
  (MonadIO m, MonadThrow m, DecodeStream (OctetStream Bytes) r) =>
  -- | .
  ValueCombinator i (WithReq m (ReqBodyStream '[OctetStream Bytes] r)) ts m
chunksBody = reqBodyStream

-- chunksBody ::
--   forall m i ts.
--   (I.Introspection i I.Request (ReqBody '[OctetStream Chunks] Bytes.Lazy.ByteString)) =>
--   (MonadIO m) =>
--   -- | .
--   ValueCombinator i (WithReq m (ReqBodyStream '[OctetStream Chunks] (IO Bytes.ByteString))) ts m
-- chunksBody = Request \_ req -> liftIO $ ReqBodyStream <$> Wai.lazyRequestBody req

module Web.Minion.Request.Body.Raw where

import Control.Monad.IO.Class
import Network.Wai qualified as Wai
import Web.Minion.Args
import Web.Minion.Introspect qualified as I
import Web.Minion.Raw
import Web.Minion.Router

lazyBytesBody ::
  forall m i ts.
  (I.Introspection i I.Request LazyBytes) =>
  (MonadIO m) =>
  -- | .
  ValueCombinator i (WithReq m LazyBytes) ts m
lazyBytesBody = Request \_ req -> liftIO $ LazyBytes <$> Wai.lazyRequestBody req

chunksBody ::
  forall m i ts.
  (I.Introspection i I.Request Chunks) =>
  (MonadIO m) =>
  -- | .
  ValueCombinator i (WithReq m Chunks) ts m
chunksBody = Request \_ req -> pure $ Chunks $ Wai.getRequestBodyChunk req

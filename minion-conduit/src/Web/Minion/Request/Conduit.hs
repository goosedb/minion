module Web.Minion.Request.Conduit (ConduitRequest (..), streamBody, streamBodyBytes) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as Bytes
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Network.Wai qualified as Wai
import Web.Minion.Args (WithReq)
import Web.Minion.Introspect qualified as I
import Web.Minion.Request
import Web.Minion.Router

newtype ConduitRequest m = ConduitRequest (C.ConduitT () Bytes.ByteString m ())

instance IsRequest (ConduitRequest m) where
  type RequestValue (ConduitRequest m) = ConduitRequest m
  getRequestValue = id

{-# INLINE streamBody #-}
streamBody ::
  forall req m i ts.
  (MonadIO m, IsRequest req, I.Introspection i I.Request req) =>
  (ConduitRequest m -> req) ->
  ValueCombinator i (WithReq m req) ts m
streamBody transform = Request \_ -> pure . transform . ConduitRequest . readReq

{-# INLINE streamBodyBytes #-}
streamBodyBytes ::
  forall m i ts.
  (MonadIO m, I.Introspection i I.Request (ConduitRequest m)) =>
  ValueCombinator i (WithReq m (ConduitRequest m)) ts m
streamBodyBytes = Request \_ -> pure . ConduitRequest . readReq

readReq :: (MonadIO m) => Wai.Request -> C.ConduitT i Bytes.ByteString m ()
readReq req = C.repeatWhileM (liftIO $ Wai.getRequestBodyChunk req) (not . Bytes.null)

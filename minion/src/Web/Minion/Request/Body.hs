module Web.Minion.Request.Body (
  DecodeBody (..),
  IsRequest (..),
  ReqBody (..),
  ReqBodyStream (..),
  Decode (..),
  Encode (..),
  EncodeStream (..),
  DecodeBodyStream (..),
  ParseBodyError (..),
  reqBody,
  reqBodyStream,
) where

import Control.Monad.Catch
import Control.Monad.IO.Class qualified as IO
import GHC.Base (Type)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Web.Minion.Args (WithReq)
import Web.Minion.Introspect qualified as I

import Web.Minion.Codec.Decode
import Web.Minion.Codec.Encode
import Web.Minion.Request
import Web.Minion.Router

newtype ReqBody (cts :: [Type]) a = ReqBody a

newtype ReqBodyStream (cts :: [Type]) a = ReqBodyStream a

instance IsRequest (ReqBody cts a) where
  type RequestValue (ReqBody cts a) = a
  getRequestValue (ReqBody a) = a

instance IsRequest (ReqBodyStream cts a) where
  type RequestValue (ReqBodyStream cts a) = a
  getRequestValue (ReqBodyStream a) = a

{- | Extracts request body with specified Content-Type

@
... '/>' 'reqBody' \@'[PlainText] \@MyRequest
@
-}
reqBody ::
  forall cts r m i ts.
  (I.Introspection i I.Request (ReqBody cts r)) =>
  (IO.MonadIO m, MonadThrow m) =>
  (DecodeBody cts r) =>
  -- | .
  ValueCombinator i (WithReq m (ReqBody cts r)) ts m
reqBody = Request \makeError req -> do
  result <- case lookup Http.hContentType $ Wai.requestHeaders req of
    Nothing -> decodeBodyFirst @cts @r (Wai.lazyRequestBody req)
    Just ct -> decodeBody @cts @r ct (Wai.lazyRequestBody req)
  case result of
    Left e -> handleError makeError req e
    Right a -> pure (ReqBody a)

handleError :: (MonadThrow m) => (Wai.Request -> MakeError) -> Wai.Request -> ParseBodyError -> m a
handleError makeError req = \case
  FailedToParse _ -> throwM $ makeError req Http.status400 "Failed to parse body"
  UnsupportedMime _ -> throwM $ makeError req Http.status415 "Unsupported Content-Type"
  InvalidMime _ -> throwM $ makeError req Http.status415 "Unsupported Content-Type"

reqBodyStream ::
  forall cts r m i ts.
  (I.Introspection i I.Request (ReqBodyStream cts r)) =>
  (IO.MonadIO m, MonadThrow m) =>
  (DecodeBodyStream cts r) =>
  -- | .
  ValueCombinator i (WithReq m (ReqBodyStream cts r)) ts m
reqBodyStream = Request \makeError req -> do
  result <- case lookup Http.hContentType $ Wai.requestHeaders req of
    Nothing -> decodeBodyFirstStream @cts @r (Wai.getRequestBodyChunk req)
    Just ct -> decodeBodyStream @cts @r ct (Wai.getRequestBodyChunk req)
  case result of
    Left e -> handleError makeError req e
    Right a -> pure (ReqBodyStream a)

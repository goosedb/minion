{-# LANGUAGE QuantifiedConstraints #-}

module Web.Minion.Response.Body (
  module Web.Minion.Response,
  RespBody (..),
  RespBodyStream (..),
  EncodeBody (..),
  Encode (..),
  handleBody,
  handleBodyStream,
) where

import Control.Exception qualified as Exc
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Class qualified as IO
import Data.ByteString qualified as Bytes
import Data.ByteString.Builder qualified as Bytes.Builder
import Data.List.NonEmpty qualified as Nel
import Data.Maybe (isJust)
import Network.HTTP.Media qualified as Http
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Web.Minion.Args
import Web.Minion.Codec.Encode
import Web.Minion.Error
import Web.Minion.Introspect qualified as I
import Web.Minion.Media
import Web.Minion.Response
import Web.Minion.Response.Status (IsStatus (..))
import Web.Minion.Router.Internal

newtype RespBody status cts a = RespBody a

newtype RespBodyStream status cts a = RespBodyStream a

instance (AllContentTypes cts) => CanRespond (RespBody status cts a) where
  canRespond = genericCanRespond @cts

instance (AllContentTypes cts) => CanRespond (RespBodyStream status cts a) where
  canRespond = genericCanRespond @cts

genericCanRespond :: forall cts. (AllContentTypes cts) => [Bytes.ByteString] -> Bool
genericCanRespond [] = True
genericCanRespond l = any (isJust . Http.matchAccept (allContentTypes @cts)) l

instance (MonadIO m) => ToResponse m (RespBody status '[] a) where
  toResponse _ (RespBody _) = IO.liftIO $ Exc.throwIO SomethingWentWrong

instance (EncodeBody (ct ': cts) a, Encode ct a, MonadIO m, ContentType ct, IsStatus status) => ToResponse m (RespBody status (ct ': cts) a) where
  toResponse [] (RespBody a) = pure $ buildBody @ct (httpStatus @status) (Nel.head $ media @ct) a
  toResponse (ct : _) (RespBody a) = pure $ encodeBody @(ct ': cts) (httpStatus @status) ct a

instance (EncodeBodyStream m (ct ': cts) a, EncodeStream m ct a, MonadIO m, ContentType ct, IsStatus status) => ToResponse m (RespBodyStream status (ct ': cts) a) where
  toResponse [] (RespBodyStream a) = encodeBodyStream @m @(ct ': cts) (httpStatus @status) (Http.renderHeader $ Nel.head $ media @ct) a
  toResponse (ct : _) (RespBodyStream a) = encodeBodyStream @m @(ct ': cts) (httpStatus @status) ct a

buildBody :: forall ct a. (Encode ct a) => Http.Status -> Http.MediaType -> a -> Wai.Response
buildBody status mt a = respond Wai.responseBuilder $ Bytes.Builder.lazyByteString (encode @ct a)
 where
  respond :: (Http.Status -> Http.ResponseHeaders -> x) -> x
  respond f = f status [(Http.hContentType, Http.renderHeader mt)]

{- | Handles request with specified HTTP method and responds with specified Content-Type

@
... '/>' 'handleBody' GET \@'[PlainText] \@MyResponse someEndpoint
@
-}
{-# INLINE handleBody #-}
handleBody ::
  forall status cts o m ts i st.
  (HandleArgs ts st m) =>
  (IsResponse m (RespBody status cts o)) =>
  (I.Introspection i I.Response (RespBody status cts o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handleBody method = makeHandle @(RespBody status cts) @o method RespBody

{-# INLINE handleBodyStream #-}
handleBodyStream ::
  forall status cts o m ts i st.
  (HandleArgs ts st m) =>
  (IsResponse m (RespBodyStream status cts o)) =>
  (I.Introspection i I.Response (RespBodyStream status cts o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handleBodyStream method = makeHandle @(RespBodyStream status cts) @o method RespBodyStream

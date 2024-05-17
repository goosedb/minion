module Web.Minion.Response.Body (
  module Web.Minion.Response,
  RespBody (..),
  EncodeBody (..),
  Encode (..),
  handleBody,
) where

import Control.Exception qualified as Exc
import Control.Exception.Base (throw)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as IO
import Data.ByteString qualified as Bytes
import Data.ByteString.Builder qualified as Bytes.Builder
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.List.NonEmpty qualified as Nel
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encode
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encode
import Network.HTTP.Media qualified as Http
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Web.Minion.Args
import Web.Minion.Error
import Web.Minion.Introspect qualified as I
import Web.Minion.Media
import Web.Minion.Media.PlainText (PlainText)
import Web.Minion.Response
import Web.Minion.Router.Internal

newtype RespBody cts a = RespBody a

instance (AllContentTypes cts) => CanRespond (RespBody cts a) where
  canRespond [] = True
  canRespond l = any (isJust . Http.matchAccept (allContentTypes @cts)) l

instance (MonadIO m) => ToResponse m (RespBody '[] a) where
  toResponse _ (RespBody _) = IO.liftIO $ Exc.throwIO SomethingWentWrong

instance (EncodeBody (ct ': cts) a, Encode ct a, MonadIO m, ContentType ct) => ToResponse m (RespBody (ct ': cts) a) where
  toResponse [] (RespBody a) = pure $ encode @ct a
  toResponse (ct : _) (RespBody a) = pure $ encodeBody @(ct ': cts) ct a

class EncodeBody cts a where
  encodeBody :: Bytes.ByteString -> a -> Wai.Response

instance EncodeBody '[] a where
  encodeBody _ _ = throw SomethingWentWrong

instance (ContentType ct, Encode ct a, EncodeBody cts a) => EncodeBody (ct ': cts) a where
  encodeBody ct a
    | Just _ <- Http.matchAccept (Nel.toList $ media @ct) ct = encode @ct a
    | otherwise = encodeBody @cts ct a

class Encode ct a where
  encode :: a -> Wai.Response

plainTextHeader :: (Http.HeaderName, Bytes.ByteString)
plainTextHeader = (Http.hContentType, Http.renderHeader $ Nel.head $ media @PlainText)

instance Encode PlainText Text.Text where
  encode a =
    Wai.responseBuilder
      Http.status200
      [plainTextHeader]
      ( Bytes.Builder.lazyByteString
          . Bytes.Lazy.fromStrict
          . Text.Encode.encodeUtf8
          $ a
      )

instance Encode PlainText Text.Lazy.Text where
  encode a =
    Wai.responseBuilder
      Http.status200
      [plainTextHeader]
      ( Bytes.Builder.lazyByteString
          . Text.Lazy.Encode.encodeUtf8
          $ a
      )

instance Encode PlainText String where
  encode a =
    Wai.responseBuilder
      Http.status200
      [plainTextHeader]
      ( Bytes.Builder.lazyByteString
          . Text.Lazy.Encode.encodeUtf8
          . Text.Lazy.pack
          $ a
      )

{- | Handles request with specified HTTP method and responds with specified Content-Type

@
... '/>' 'handleBody' GET \@'[PlainText] \@MyResponse someEndpoint
@
-}
{-# INLINE handleBody #-}
handleBody ::
  forall cts o m ts i st.
  (HandleArgs ts st m) =>
  (IsResponse m (RespBody cts o)) =>
  (I.Introspection i I.Response (RespBody cts o)) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handleBody method = makeHandle @(RespBody cts) @o method RespBody

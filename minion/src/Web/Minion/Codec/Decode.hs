module Web.Minion.Codec.Decode where

import Control.Monad ((>=>))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Functor
import Data.List.NonEmpty qualified as Nel
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encode
import Network.HTTP.Media qualified as Http
import Web.FormUrlEncoded (FromForm)
import Web.FormUrlEncoded qualified as Http
import Web.Minion.Media (ContentType (..))
import Web.Minion.Media.FormUrlEncoded
import Web.Minion.Media.PlainText

data ParseBodyError = UnsupportedMime Http.MediaType | InvalidMime (Maybe Bytes.ByteString) | FailedToParse Text

class Decode ct a where
  decode :: Bytes.Lazy.ByteString -> Either Text.Text a

class DecodeStream ct a where
  decodeStream :: IO Bytes.ByteString -> IO (Either Text.Text a)

instance Decode PlainText Text.Text where
  decode = Right . Text.Lazy.toStrict . Text.Lazy.Encode.decodeUtf8

instance Decode PlainText Text.Lazy.Text where
  decode = Right . Text.Lazy.Encode.decodeUtf8

instance Decode PlainText String where
  decode = fmap Text.Lazy.unpack . decode @PlainText

instance (FromForm a) => Decode FormUrlEncoded a where
  decode = Http.urlDecodeForm >=> Http.fromForm

class DecodeBody cts a where
  decodeBody ::
    (MonadIO m, MonadThrow m) =>
    -- | Content-Type header value
    Bytes.ByteString ->
    -- | Request body
    IO Bytes.Lazy.ByteString ->
    m (Either ParseBodyError a)

  decodeBodyFirst ::
    forall m.
    (MonadIO m, MonadThrow m) =>
    -- | Request body
    IO Bytes.Lazy.ByteString ->
    m (Either ParseBodyError a)

class DecodeBodyStream cts a where
  decodeBodyStream ::
    (MonadIO m, MonadThrow m) =>
    -- | Content-Type header value
    Bytes.ByteString ->
    -- | Chunked request body
    IO Bytes.ByteString ->
    m (Either ParseBodyError a)

  decodeBodyFirstStream ::
    forall m.
    (MonadIO m, MonadThrow m) =>
    -- | Chunked request body
    IO Bytes.ByteString ->
    m (Either ParseBodyError a)

instance DecodeBody '[] a where
  decodeBody h _ = pure $ Left $ maybe (InvalidMime (Just h)) UnsupportedMime (Http.parseAccept @Http.MediaType h)
  decodeBodyFirst _ = pure $ Left (InvalidMime Nothing)

instance DecodeBodyStream '[] a where
  decodeBodyStream h _ = pure $ Left $ maybe (InvalidMime (Just h)) UnsupportedMime (Http.parseAccept @Http.MediaType h)
  decodeBodyFirstStream _ = pure $ Left (InvalidMime Nothing)

instance (ContentType ct, Decode ct a, DecodeBody cts a) => DecodeBody (ct ': cts) a where
  decodeBody contentType body
    | Just _ <- Http.matchAccept (Nel.toList $ media @ct) contentType = parseCt @ct @a body
    | otherwise = do
        decodeBody @cts contentType body <&> either Left Right
  decodeBodyFirst = parseCt @ct

instance (ContentType ct, DecodeStream ct a, DecodeBodyStream cts a) => DecodeBodyStream (ct ': cts) a where
  decodeBodyStream contentType body
    | Just _ <- Http.matchAccept (Nel.toList $ media @ct) contentType = parseCtStream @ct @a body
    | otherwise = do
        decodeBodyStream @cts contentType body <&> either Left Right
  decodeBodyFirstStream = parseCtStream @ct

parseCt :: forall ct a m. (MonadIO m, MonadThrow m, Decode ct a) => IO Bytes.Lazy.ByteString -> m (Either ParseBodyError a)
parseCt body =
  liftIO body
    >>= either
      (pure . Left . FailedToParse)
      (pure . Right)
      . decode @ct @a

parseCtStream :: forall ct a m. (MonadIO m, MonadThrow m, DecodeStream ct a) => IO Bytes.ByteString -> m (Either ParseBodyError a)
parseCtStream chunks =
  either
    (Left . FailedToParse)
    Right
    <$> liftIO (decodeStream @ct @a chunks)

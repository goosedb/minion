module Web.Minion.Codec.Encode where

import Control.Exception
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as Bytes
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Builder qualified as Bytes.Builder
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.List.NonEmpty qualified as Nel
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encode
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encode
import Network.HTTP.Media qualified as Http
import Network.HTTP.Types qualified as Http
import Network.HTTP.Types.Header qualified as Http
import Network.Wai qualified as Wai
import Web.Minion.Error (SomethingWentWrong (..))
import Web.Minion.Media
import Web.Minion.Media.Html (Html)
import Web.Minion.Media.OctetStream
import Web.Minion.Media.PlainText

class Encode ct a where
  encode :: a -> Bytes.Lazy.ByteString

class EncodeStream m ct a where
  encodeStream :: a -> m Wai.StreamingBody

instance (Monad m) => EncodeStream m (OctetStream Chunks) (IO Bytes.ByteString) where
  encodeStream chunks = pure \write flush ->
    let loop =
          chunks >>= \case
            "" -> flush
            chunk -> write (Builder.byteString chunk) >> flush >> loop
     in loop

class EncodeBody cts a where
  encodeBody :: Http.Status -> Bytes.ByteString -> a -> Wai.Response

instance EncodeBody '[] a where
  encodeBody _ _ = throw SomethingWentWrong

instance (ContentType ct, Encode ct a, EncodeBody cts a) => EncodeBody (ct ': cts) a where
  encodeBody status ct a
    | Just mt <- Http.matchAccept (Nel.toList $ media @ct) ct =
        respond mt Wai.responseBuilder $ Bytes.Builder.lazyByteString (encode @ct a)
    | otherwise = encodeBody @cts status ct a
   where
    respond mt f = f status [(Http.hContentType, Http.renderHeader mt)]

class EncodeBodyStream m cts a where
  encodeBodyStream :: Http.Status -> Bytes.ByteString -> a -> m Wai.Response

instance EncodeBodyStream m '[] a where
  encodeBodyStream _ _ = throw SomethingWentWrong

instance (MonadIO m, ContentType ct, EncodeStream m ct a, EncodeBodyStream m cts a) => EncodeBodyStream m (ct ': cts) a where
  encodeBodyStream status ct a
    | Just mt <- Http.matchAccept (Nel.toList $ media @ct) ct = do
        stream <- encodeStream @m @ct a
        pure $ respond mt Wai.responseStream stream
    | otherwise = encodeBodyStream @m @cts status ct a
   where
    respond mt f = f status [(Http.hTransferEncoding, "chunked"), (Http.hContentType, Http.renderHeader mt)]

instance Encode PlainText Text.Text where
  encode = Bytes.Lazy.fromStrict . Text.Encode.encodeUtf8

instance Encode (OctetStream Bytes) Bytes.ByteString where
  encode = Bytes.Lazy.fromStrict

instance Encode (OctetStream Bytes) Bytes.Lazy.ByteString where
  encode = id

instance Encode PlainText Text.Lazy.Text where
  encode = Text.Lazy.Encode.encodeUtf8

instance Encode PlainText String where
  encode = Text.Lazy.Encode.encodeUtf8 . Text.Lazy.pack

instance Encode Html String where
  encode = Text.Lazy.Encode.encodeUtf8 . Text.Lazy.pack

instance Encode Html Bytes.ByteString where
  encode = Bytes.Lazy.fromStrict

instance Encode Html Bytes.Lazy.ByteString where
  encode = id

instance Encode Html Text.Lazy.Text where
  encode = Text.Lazy.Encode.encodeUtf8

instance Encode Html Text.Text where
  encode = Bytes.Lazy.fromStrict . Text.Encode.encodeUtf8

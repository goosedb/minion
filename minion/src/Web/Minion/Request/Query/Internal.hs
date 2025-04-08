module Web.Minion.Request.Query.Internal where

import Control.Monad.Catch (MonadThrow (throwM))
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.List.NonEmpty qualified as Nel
import Data.Maybe (catMaybes)
import Data.String.Conversions (ConvertibleStrings (convertString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encode
import Data.Text.Encoding qualified as Text.Encoding
import Network.HTTP.Types qualified as Http
import Web.HttpApiData (FromHttpApiData)
import Web.Internal.HttpApiData (FromHttpApiData (..))
import Web.Minion.Args.Internal
import Web.Minion.Error (ServerError)
import Web.Minion.Introspect qualified as I
import Web.Minion.Router.Internal

type QueryParamName = Text.Text

withQueryParam ::
  forall a presence parsing i m ts.
  (I.Introspection i I.QueryParam a, IsRequired presence, IsLenient parsing) =>
  Text ->
  (MakeError -> Maybe (Nel.NonEmpty (Maybe Bytes.ByteString)) -> m (Arg presence parsing a)) ->
  Router' i (ts :+ WithQueryParam presence parsing m a) m ->
  Router' i ts m
withQueryParam qn = QueryParam (Text.Encode.encodeUtf8 qn)

noKeyError :: Text -> Bytes.Lazy.ByteString
noKeyError qn = convertString $ "Query param not found: " <> qn

noValueError :: Text -> Bytes.Lazy.ByteString
noValueError qn = convertString $ "Query param value not found: " <> qn

badReq :: (MonadThrow m) => (Http.Status -> Bytes.Lazy.ByteString -> ServerError) -> (Text -> Bytes.Lazy.ByteString) -> Text -> m a
badReq makeError err qn = throwM $ makeError Http.status400 $ err qn

tryDecode :: (FromHttpApiData a) => MakeError -> Bytes.ByteString -> Either ServerError a
tryDecode makeError = first (makeError Http.status400 . convertString) . decodeQueryParam

decodeQueryParam :: (FromHttpApiData a) => Bytes.ByteString -> Either Text a
decodeQueryParam = parseQueryParam . Text.Encoding.decodeUtf8

filterNothings :: Nel.NonEmpty (Maybe a) -> Maybe (Nel.NonEmpty a)
filterNothings = Nel.nonEmpty . catMaybes . Nel.toList

parseQueryFlag :: Bytes.ByteString -> Bool
parseQueryFlag = flip (elem @[]) ["1", "true", ""]

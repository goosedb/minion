module Web.Minion.Client.Types where

import Control.Exception (SomeException, finally)
import Data.ByteString.Base64 qualified as Bytes.Base64
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Functor (void, (<&>))
import Data.Kind (Type)
import Data.Kind qualified as Kind
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics
import Network.HTTP.Client qualified as Http
import Network.HTTP.Media qualified as Http
import Network.HTTP.Types qualified as Http
import Web.HttpApiData qualified as Http
import Web.Minion (RespBody)
import Web.Minion.Auth (Auth)
import Web.Minion.Auth.Basic (Basic, BasicAuth (..), Password (..), Username (..))
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Body (DecodeBody (decodeBody), Encode (..), ParseBodyError (..), ReqBody)
import Web.Minion.Response.Header (AddHeaders)
import Web.Minion.Response.Status (IsStatus (..))
import Web.Minion.Response.Union (Inject (inject), Union (..))

data Client

data ClientError
  = FailureResponse (Http.Response ResponseStream)
  | DecodeFailure Text (Http.Response ())
  | UnsupportedContentType Http.MediaType (Http.Response ())
  | InvalidContentTypeHeader (Http.Response ())
  | ConnectionError SomeException
  | UnexpectedCode (Http.Response ResponseStream)
  deriving (Generic, Typeable, Show)

newtype ResponseStream = ResponseStream Http.BodyReader

class (Typeable v, Http.ToHttpApiData v) => QueryParamClient v
instance (Typeable v, Http.ToHttpApiData v) => QueryParamClient v

class (Typeable v, Http.ToHttpApiData v) => CaptureClient v
instance (Typeable v, Http.ToHttpApiData v) => CaptureClient v

data HeaderClientValue v where
  ConstantHeaderValue :: (Http.ToHttpApiData v) => v -> HeaderClientValue v
  NoHeaderValue :: HeaderClientValue Void
  PassedHeaderValue :: (Http.ToHttpApiData v) => HeaderClientValue v

data AnyHeaderClientValue where
  AnyHeaderClientValue :: HeaderClientValue v -> AnyHeaderClientValue

class (Typeable (HeaderFromClient v), Typeable v, Http.ToHttpApiData (HeaderFromClient v)) => HeaderClient v where
  type HeaderFromClient v :: Kind.Type
  headerValue :: HeaderClientValue (HeaderFromClient v)

class (Typeable (RequestFromClient v), Typeable v) => RequestClient v where
  type RequestFromClient v :: Kind.Type
  packRequest :: RequestFromClient v -> Http.Request -> IO Http.Request

instance (Typeable a, Typeable cts, Typeable ct, Encode ct a) => RequestClient (ReqBody (ct ': cts) a) where
  type RequestFromClient (ReqBody (ct ': cts) a) = a
  packRequest a req = pure $ req{Http.requestBody = Http.RequestBodyLBS $ encode @ct a}

instance (ApplyAuths auths, Typeable (AnyAuth auths), Typeable auths, Typeable a) => RequestClient (Auth auths a) where
  type RequestFromClient (Auth auths a) = Union (AnyAuth auths)
  packRequest = applyAuths @auths

class ApplyAuths auths where
  applyAuths :: Union (AnyAuth auths) -> Http.Request -> IO Http.Request

instance (ApplyAuths auths, ApplyAuth auth) => ApplyAuths (auth ': auths) where
  applyAuths (This a) req = applyAuth @auth a req
  applyAuths (That a) req = applyAuths @auths a req

instance ApplyAuths '[] where
  applyAuths _ _ = error "impossible"

class ApplyAuth auth where
  type AuthParam auth :: Type
  applyAuth :: AuthParam auth -> Http.Request -> IO Http.Request

instance ApplyAuth Basic where
  type AuthParam Basic = BasicAuth
  applyAuth BasicAuth{username = Username username, password = Password password} req =
    pure
      let creds = Bytes.Base64.encode username <> ":" <> Bytes.Base64.encode password
       in req{Http.requestHeaders = (Http.hAuthorization, "Basic " <> creds) : Http.requestHeaders req}

type family AnyAuth auths where
  AnyAuth '[] = '[]
  AnyAuth (a ': as) = AuthParam a ': AnyAuth as

instance Show ResponseStream where
  show _ = "<bytes>"

class (Typeable v) => ResponseClient v where
  type ResponseForClient v :: Kind.Type
  acceptResponse :: Http.Response Http.BodyReader -> IO (Either ClientError (ResponseForClient v))
instance (Typeable (RespBody status cts a), DecodeBody cts a, IsStatus status) => ResponseClient (RespBody status cts a) where
  type ResponseForClient (RespBody status cts a) = a

  acceptResponse resp =
    let accept = do
          let ct = lookup Http.hContentType $ Http.responseHeaders resp
          let body = Bytes.Lazy.fromChunks <$> (Http.brConsume (Http.responseBody resp) `finally` Http.responseClose resp)
          decodeBody @cts @a (fromMaybe mempty ct) body <&> \case
            Left (FailedToParse e) -> Left (DecodeFailure e (void resp))
            Left (UnsupportedMime m) -> Left (UnsupportedContentType m (void resp))
            Left (InvalidMime _) -> Left $ InvalidContentTypeHeader (void resp)
            Right a -> Right a
     in if Http.responseStatus resp == httpStatus @status then accept else pure $ Left $ UnexpectedCode (ResponseStream <$> resp)

instance (ResponseClient a, Typeable hs) => ResponseClient (AddHeaders hs a) where
  type ResponseForClient (AddHeaders hs a) = ResponseForClient a
  acceptResponse = acceptResponse @a

type family SomeBody resps where
  SomeBody '[] = '[]
  SomeBody (a ': as) = ResponseForClient a ': SomeBody as

instance
  ( Typeable r
  , Typeable rs
  , ResponseClient r
  , ResponseClient (Union rs)
  , Union (SomeBody rs)
      ~ ResponseForClient (Union rs)
  ) =>
  ResponseClient (Union (r : rs))
  where
  type ResponseForClient (Union (r ': rs)) = Union (SomeBody (r ': rs))
  acceptResponse resp =
    acceptResponse @r resp >>= \case
      Left (UnsupportedContentType _ _) -> fmap That <$> acceptResponse @(Union rs) resp
      Left (UnexpectedCode _) -> fmap That <$> acceptResponse @(Union rs) resp
      Left e -> pure $ Left e
      Right v -> pure $ Right $ inject v

instance ResponseClient (Union '[]) where
  type ResponseForClient (Union '[]) = Union '[]

  acceptResponse = pure . Left . DecodeFailure "Failed to accept Union response" . void

instance I.HasIntrospection Client where
  type IntrospectionFor Client I.QueryParam = QueryParamClient
  type IntrospectionFor Client I.Capture = CaptureClient
  type IntrospectionFor Client I.Captures = CaptureClient
  type IntrospectionFor Client I.Header = HeaderClient
  type IntrospectionFor Client I.Request = RequestClient
  type IntrospectionFor Client I.Response = ResponseClient
  type IntrospectionFor Client I.Description = I.AbsolutelyNothing

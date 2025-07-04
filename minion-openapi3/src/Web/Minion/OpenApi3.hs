module Web.Minion.OpenApi3 (
  OpenApi3,
  AttachRequestSchema (..),
  IsOpenApi3Description (..),
  OpenApi3Description (..),
  ToResponses (..),
  generateOpenApi3,
  {- | Use these newtypes to implement instances for according auths/response bodies/request bodies
  We do not implement it for concrete types to avoid extra dependencies
  -}
  AsCookieJwt (..),
  AsJwt (..),
  AsHtml (..),
  AsSSE (..),
  AsBinary (..),
  AsMultipart (..),
  AttachSecuritySchema (..),
  AttachSecuritySchemas (..),
) where

import Data.OpenApi hiding (Header (..))
import Web.Minion hiding (description, status)
import Web.Minion.Router

import Control.Arrow ((>>>))
import Control.Lens hiding (index)
import Data.ByteString qualified as Bytes
import Data.CaseInsensitive qualified as CI
import Data.Data (Proxy (..))
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet.InsOrd qualified as InsOrdHashSet
import Data.OpenApi.Declare (runDeclare)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as Http
import Web.Minion.Auth.Basic (Basic)
import Web.Minion.Introspect qualified as I
import Web.Minion.Media
import Web.Minion.Response (Redirect)
import Web.Minion.Response.Header (AddHeaders)
import Web.Minion.Response.Status
import Web.Minion.Response.Union

data OpenApi3

class AttachRequestSchema a where
  attachRequestSchema :: OpenApi -> OpenApi

instance I.HasIntrospection OpenApi3 where
  type IntrospectionFor OpenApi3 I.QueryParam = ToParamSchema
  type IntrospectionFor OpenApi3 I.Capture = ToParamSchema
  type IntrospectionFor OpenApi3 I.Captures = ToParamSchema
  type IntrospectionFor OpenApi3 I.Header = ToParamSchema
  type IntrospectionFor OpenApi3 I.Request = AttachRequestSchema
  type IntrospectionFor OpenApi3 I.Response = ToResponses
  type IntrospectionFor OpenApi3 I.Description = IsOpenApi3Description

class IsOpenApi3Description a where
  toOpenApi3Description :: a -> OpenApi3Description

instance IsOpenApi3Description OpenApi3Description where
  toOpenApi3Description = id

data OpenApi3Description = DescriptionText Text | SummaryText Text | TagText Text
  deriving (Eq, Ord, Show)

instance (AttachSecuritySchemas as) => AttachRequestSchema (Auth as a) where
  attachRequestSchema = attachSecuritySchemas @as

class AttachSecuritySchemas as where
  attachSecuritySchemas :: OpenApi -> OpenApi

instance AttachSecuritySchemas '[] where
  attachSecuritySchemas = id

instance (AttachSecuritySchema a, AttachSecuritySchemas as) => AttachSecuritySchemas (a ': as) where
  attachSecuritySchemas = attachSecuritySchemas @as . attachSecuritySchema @a

class AttachSecuritySchema a where
  attachSecuritySchema :: OpenApi -> OpenApi

instance AttachSecuritySchema Basic where
  attachSecuritySchema = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
   where
    identifier = "BasicAuth"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp HttpSchemeBasic
        , _securitySchemeDescription = Just "Basic Authentication"
        }

newtype AsJwt a = AsJwt a
instance AttachSecuritySchema (AsJwt a) where
  attachSecuritySchema = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
   where
    identifier = "JWT"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
        , _securitySchemeDescription = Just "Bearer Authentication"
        }

newtype AsCookieJwt a = AsCookieJwt a

instance AttachSecuritySchema (AsCookieJwt a) where
  attachSecuritySchema = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
   where
    identifier = "Cookie"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
        , _securitySchemeDescription = Just "Cookie Authentication"
        }

addSecurityScheme :: Text -> SecurityScheme -> OpenApi -> OpenApi
addSecurityScheme securityIdentifier securityScheme openApi =
  openApi
    { _openApiComponents =
        (_openApiComponents openApi)
          { _componentsSecuritySchemes =
              _componentsSecuritySchemes (_openApiComponents openApi)
                <> SecurityDefinitions (HM.singleton securityIdentifier securityScheme)
          }
    }

addSecurityRequirement :: Text -> OpenApi -> OpenApi
addSecurityRequirement securityRequirement =
  allOperations
    . security
    %~ ((SecurityRequirement $ HM.singleton securityRequirement []) :)

instance (ToSchema a, AllContentTypes cts) => AttachRequestSchema (ReqBody cts a) where
  attachRequestSchema =
    addRequestBody reqB
      >>> addDefaultResponse400 tname
      >>> components . schemas %~ (<> defs)
   where
    addRequestBody rb = allOperations . requestBody ?~ Inline rb
    tname = "body"
    (defs, ref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty
    reqB =
      (mempty :: RequestBody)
        & content .~ InsOrdHashMap.fromList [(t, mempty & schema ?~ ref) | t <- allContentTypes @cts]

newtype AsHtml a = AsHtml a
instance ToResponses (AsHtml a) where
  toResponses = (resps, [])
   where
    resps =
      Responses
        { _responsesResponses = mempty & at 200 ?~ resp
        , _responsesDefault = Nothing
        }
    resp =
      Inline
        ( mempty
            & content
              .~ InsOrdHashMap.fromList
                [("text/html", mempty)]
        )

newtype AsBinary a = AsBinary a
instance ToResponses (AsBinary a) where
  toResponses = (resps, [])
   where
    resps =
      Responses
        { _responsesResponses = mempty & at 200 ?~ resp
        , _responsesDefault = Nothing
        }
    resp =
      Inline
        ( mempty
            & content
              .~ InsOrdHashMap.fromList
                [("octet/stream", mempty)]
        )

newtype AsSSE a = AsSSE a

-- | It's just a stub for now
instance ToResponses (AsSSE a) where
  toResponses = (resps, [])
   where
    resps =
      Responses
        { _responsesResponses = mempty & at 200 ?~ resp
        , _responsesDefault = Nothing
        }
    resp =
      Inline
        ( mempty
            & content
              .~ InsOrdHashMap.fromList
                [("text/event-stream", mempty)]
        )

newtype AsMultipart a = AsMultipart a

instance AttachRequestSchema (AsMultipart a) where
  attachRequestSchema =
    addRequestBody reqB
      >>> addDefaultResponse400 tname
   where
    addRequestBody rb = allOperations . requestBody ?~ Inline rb
    tname = "body"
    reqB =
      (mempty :: RequestBody)
        & content .~ InsOrdHashMap.fromList [("multipart/form-data", mempty)]

instance (ToSchema a, AllContentTypes cts, IsStatus status) => ToResponses (RespBody status cts a) where
  toResponses = (resps, defs)
   where
    (defs, ref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty
    resps =
      Responses
        { _responsesResponses = mempty & at (Http.statusCode $ httpStatus @status) ?~ resp
        , _responsesDefault = Nothing
        }
    resp =
      Inline
        ( mempty
            & content
              .~ InsOrdHashMap.fromList
                [(t, mempty & schema ?~ ref) | t <- responseContentTypes]
        )

    responseContentTypes = allContentTypes @cts

instance (IsStatus status) => ToResponses (NoBody status) where
  toResponses = (resps, mempty)
   where
    resps =
      Responses
        { _responsesResponses = mempty & at (Http.statusCode $ httpStatus @status) ?~ resp
        , _responsesDefault = Nothing
        }
    resp = Inline mempty

instance (IsStatus status) => ToResponses Redirect where
  toResponses = (resps, mempty)
   where
    resps =
      Responses
        { _responsesResponses = mempty & at 302 ?~ resp
        , _responsesDefault = Nothing
        }
    resp = Inline mempty

instance (ToResponses a, ToResponses (Union as)) => ToResponses (Union (a ': as)) where
  toResponses =
    let (resp, def) = toResponses @a
        (resps, defs) = toResponses @(Union as)
     in (resp <> resps, def <> defs)

instance ToResponses (Union '[]) where
  toResponses = (mempty, mempty)

instance (ToResponses a) => ToResponses (AddHeaders '[] a) where
  toResponses = toResponses @a

instance (ToResponses a) => ToResponses (AddHeaders hs a) where
  toResponses = toResponses @a

class ToResponses a where
  toResponses :: (Responses, Definitions Schema)

generateOpenApi3 :: forall m i ts. (I.Elem OpenApi3 i) => Router' i ts m -> OpenApi
generateOpenApi3 = \case
  Capture @a _ pname r -> wi @I.Capture @a do
    generateOpenApi3 r & openapi3Capture @a pname
  Captures @a _ pname r -> wi @I.Captures @a do
    generateOpenApi3 r & openapi3Capture @a pname
  Header @a @presence hname _ r -> wi @I.Header @a do
    generateOpenApi3 r & opeanapi3Header @presence @a hname
  Request @r _ r -> wi @I.Request @r do
    generateOpenApi3 r & attachRequestSchema @r
  HideIntrospection @_ @i' rest -> I.withElem @OpenApi3 @i' mempty (generateOpenApi3 rest)
  Piece path r -> prependPath (Text.unpack path) (generateOpenApi3 r)
  Middleware _ r -> generateOpenApi3 r
  Alt rs -> foldMap generateOpenApi3 rs
  MapArgs _ r -> generateOpenApi3 r
  Description @desc anyDesc r -> wi @I.Description @desc do
    case toOpenApi3Description anyDesc of
      DescriptionText txt -> generateOpenApi3 r & allOperations . description %~ (Just txt <>)
      SummaryText txt -> generateOpenApi3 r & allOperations . summary %~ (Just txt <>)
      TagText txt -> generateOpenApi3 r & allOperations . tags %~ InsOrdHashSet.insert txt
  QueryParam @a @presence bname _ r -> wi @I.QueryParam @a do
    generateOpenApi3 r & openapi3QueryParam @presence @a bname
  Handle @o httpMethod _ -> wi @I.Response @o do
    let
      method :: Lens' PathItem (Maybe Operation)
      method = case httpMethod of
        "GET" -> get
        "POST" -> post
        "PATCH" -> patch
        "DELETE" -> delete
        "PUT" -> put
        "TRACE" -> trace
        "OPTIONS" -> options
        "HEAD" -> head_
        _ -> lens (const Nothing) const
      (resp, defs) = toResponses @o
     in
      mempty
        & paths . at "/" ?~ (mempty & method ?~ (mempty{_operationResponses = resp}))
        & components . schemas .~ defs
  Raw _ -> mempty & paths . at "/" ?~ mempty
 where
  wi :: forall t x a. (I.Introspection i t x) => ((I.IntrospectionFor OpenApi3 t x) => a) -> a
  wi = I.withIntrospection @OpenApi3 @i @t @x

openapi3QueryParam :: forall presence a. (IsRequired presence, ToParamSchema a) => Bytes.ByteString -> OpenApi -> OpenApi
openapi3QueryParam bname = addParam param >>> addDefaultResponse400 tname
 where
  tname = Text.decodeUtf8 bname
  param =
    mempty
      & name .~ tname
      & required ?~ isRequired @presence
      & in_ .~ ParamQuery
      & schema ?~ Inline (toParamSchema (Proxy @a))

openapi3Capture :: forall a. (ToParamSchema a) => Text -> OpenApi -> OpenApi
openapi3Capture tname =
  addParam param
    >>> prependPath capture_
    >>> addDefaultResponse404 tname
 where
  sname = Text.unpack tname
  capture_ = "{" <> sname <> "}"
  param =
    mempty
      & name .~ tname
      & required ?~ True
      & in_ .~ ParamPath
      & schema ?~ Inline (toParamSchema (Proxy :: Proxy a))

opeanapi3Header :: forall presence a. (IsRequired presence, ToParamSchema a) => Http.HeaderName -> OpenApi -> OpenApi
opeanapi3Header hname =
  addParam param
    >>> addDefaultResponse400 tname
 where
  tname = Text.decodeUtf8 $ CI.original hname
  param =
    mempty
      & name .~ tname
      & required ?~ isRequired @presence
      & in_ .~ ParamHeader
      & schema ?~ Inline (toParamSchema (Proxy :: Proxy a))

addParam :: Param -> OpenApi -> OpenApi
addParam param = allOperations . parameters %~ (Inline param :)

addDefaultResponse404 :: ParamName -> OpenApi -> OpenApi
addDefaultResponse404 pname = setResponseWith (\old _new -> alter404 old) 404 (return response404)
 where
  sname = markdownCode pname
  description404 = sname <> " not found"
  alter404 = description %~ ((sname <> " or ") <>)
  response404 = mempty & description .~ description404

addDefaultResponse400 :: ParamName -> OpenApi -> OpenApi
addDefaultResponse400 pname = setResponseWith (\old _new -> alter400 old) 400 (return response400)
 where
  sname = markdownCode pname
  description400 = "Invalid " <> sname
  alter400 = description %~ (<> (" or " <> sname))
  response400 = mempty & description .~ description400

markdownCode :: Text -> Text
markdownCode s = "`" <> s <> "`"

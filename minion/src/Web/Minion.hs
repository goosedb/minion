{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Web.Minion (
  -- * Minion
  Router',
  Router,
  MakeError,

  -- * Combinators
  ValueCombinator,
  Combinator,
  alt,
  (/>),
  (.>),
  (!>),
  hideIntrospection,
  description,

  -- ** Header
  module Web.Minion.Request.Header,

  -- ** Query params
  module Web.Minion.Request.Query,

  -- ** URL
  module Web.Minion.Request.Url,

  -- ** Request
  ReqBody (..),
  reqBody,
  reqPlainText,
  reqFormUrlEncoded,
  reqJson,
  LazyBytes (..),
  lazyBytesBody,
  Chunks (..),
  chunksBody,

  -- ** Response
  NoBody (..),
  ToResponse (..),
  CanRespond (..),

  -- ** Handler
  handle,
  handlePP,
  handleJson,
  handlePlainText,
  RespBody (..),
  handleBody,
  module Web.Minion.Request.Method,

  -- ** Middleware
  MiddlewareM,
  middleware,

  -- ** Server
  ApplicationM,
  MinionSettings (..),
  MatchedData (..),
  MatchedPiece (..),
  MatchedHeader (..),
  MatchedQuery (..),
  serve,
  serveWithSettings,
  defaultMinionSettings,
  defaultErrorBuilders,

  -- ** Exceptions
  NoMatch (..),
  SomethingWentWrong (..),
  ServerError (..),

  -- ** Args
  module Web.Minion.Args,

  -- ** Auth
  module Web.Minion.Auth,

  -- ** Reexports
  Void,
  Exc.MonadCatch (..),
  Exc.MonadThrow (..),
) where

import Control.Monad ((>=>))
import Control.Monad.Catch qualified as Exc
import Control.Monad.IO.Class qualified as IO
import Data.Binary.Builder qualified as Bytes.Builder
import Data.Text qualified as Text
import Data.Void (Void)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Http
import Network.Wai qualified as Wai
import Web.Minion.Args
import Web.Minion.Auth
import Web.Minion.Error (
  ErrorBuilders (..),
  NoMatch (..),
  ServerError (..),
  SomethingWentWrong (..),
 )

import GHC.Exts (IsList (..))
import Web.Minion.Introspect qualified as I
import Web.Minion.Json (handleJson, reqJson)
import Web.Minion.Raw
import Web.Minion.Request.Body (ReqBody (..), reqBody)
import Web.Minion.Request.Body.FormUrlEncoded
import Web.Minion.Request.Body.PlainText
import Web.Minion.Request.Body.Raw
import Web.Minion.Request.Header
import Web.Minion.Request.Method
import Web.Minion.Request.Query
import Web.Minion.Request.Url
import Web.Minion.Response
import Web.Minion.Response.Body (RespBody (RespBody), handleBody)
import Web.Minion.Response.Body.PlainText (handlePlainText)
import Web.Minion.Router.Internal

-- | Use it if you don't care about value captured by previous combinator
{-# INLINE (!>) #-}
(!>) ::
  (Router' i (ts :+ x) r -> Router' i ts r) ->
  -- | .
  Router' i (ts :+ Hide x) r ->
  Router' i ts r
a !> b = a .> MapArgs (\case (x :#! as) -> Hide x :#! as) .> b

-- | Use it after 'Combinator'
{-# INLINE (/>) #-}
(/>) ::
  (Router' i ts r -> Router' i ts r) ->
  -- | .
  Router' i ts r ->
  Router' i ts r
(/>) = id

-- | Use it after 'ValueCombinator' and 'MapArgs'
{-# INLINE (.>) #-}
(.>) ::
  (Router' i ts' r -> Router' i ts r) ->
  -- | .
  Router' i ts' r ->
  Router' i ts r
(.>) = id

infixr 0 .>

infixr 0 />

infixr 0 !>

{- | Could be omitted with `OverloadedLists`

@
{\-# LANGUAGE OverloadedLists #-\}
"foo" '/>'
  [ "bar" '/>' ...
  , "baz" '/>' ...
  ]
@

@
{\-# LANGUAGE NoOverloadedLists #-\}
"foo" '/>' 'alt'
  [ "bar" '/>' ...
  , "baz" '/>' ...
  ]
@
-}
{-# INLINE alt #-}
alt :: [Router' i ts r] -> Router' i ts r
alt = fromList

{- | Handles request with specified HTTP method

@
... '/>' 'handle' \@MyResponse GET someEndpoint
@
-}
{-# INLINE handle #-}
handle ::
  forall o m ts i st.
  ( HandleArgs ts st m
  , ToResponse m o
  , CanRespond o
  , I.Introspection i I.Response o
  ) =>
  -- | .
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
handle = handlePP @o @o @m @m id

{-# INLINE handlePP #-}
handlePP ::
  forall a o n m ts i st.
  ( HandleArgs ts st m
  , ToResponse m o
  , CanRespond o
  , I.Introspection i I.Response o
  ) =>
  -- | post process
  (n a -> m o) ->
  Http.Method ->
  (DelayedArgs st ~> n a) ->
  Router' i ts m
handlePP nt method f = Handle @o method (nt . apply f)

-- | Add description for route
description :: (I.Introspection i I.Description a) => a -> Combinator i ts m
description = Description

hideIntrospection :: Router' i ts m -> Router' i' ts m
hideIntrospection = HideIntrospection

{- | Injects middleware

@
... '/>' 'middleware' Wai.realIp '/>' ...
@
-}
middleware :: MiddlewareM m -> Combinator i ts m
middleware = Middleware

data MinionSettings m = MinionSettings
  { notFound :: m Wai.Response
  , httpError :: ServerError -> m Wai.Response
  , errorBuilders :: ErrorBuilders
  , withMatchedData :: forall a. MatchedData -> m a -> m a
  }

{-# INLINE serve #-}
serve :: (IO.MonadIO m, Exc.MonadCatch m) => Router' i Void m -> ApplicationM m
serve = serveWithSettings defaultMinionSettings

defaultMinionSettings :: (IO.MonadIO m, Exc.MonadCatch m) => MinionSettings m
defaultMinionSettings =
  MinionSettings
    { notFound = pure (Wai.responseBuilder Http.status404 [] mempty)
    , httpError = \ServerError{..} -> pure $ Wai.responseBuilder status headers (Bytes.Builder.fromLazyByteString body)
    , errorBuilders = defaultErrorBuilders
    , withMatchedData = \_ x -> x
    }

defaultErrorBuilders :: ErrorBuilders
defaultErrorBuilders =
  ErrorBuilders
    { headerErrorBuilder = defaultBuilder
    , queryParamsErrorBuilder = defaultBuilder
    , captureErrorBuilder = defaultBuilder
    , bodyErrorBuilder = defaultBuilder
    }
 where
  defaultBuilder _ status = ServerError status []

-- | The same as 'serve' but allows to configure exceptions handlers
{-# INLINE serveWithSettings #-}
serveWithSettings :: (IO.MonadIO m, Exc.MonadCatch m) => MinionSettings m -> Router' i Void m -> ApplicationM m
serveWithSettings MinionSettings{..} router req resp =
  Exc.catches @[]
    (route withMatchedData errorBuilders (RoutingState (filter (not . Text.null) $ Http.pathInfo req) [] [] []) RHNil router req resp)
    [ Exc.Handler \(NoMatch e) -> maybe notFound httpError e >>= IO.liftIO . resp
    , Exc.Handler $ httpError >=> IO.liftIO . resp
    ]

{-# LANGUAGE PartialTypeSignatures #-}

module Web.Minion.Router.Internal where

import Control.Monad ((>=>))
import Control.Monad.IO.Class qualified as IO
import Data.ByteString (ByteString)
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Exts (IsList (..))
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Http
import Network.Wai qualified as Wai

import Control.Exception qualified as IOExc
import Control.Monad.Catch qualified as Exc
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Kind (Type)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as Nel
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)
import Web.Minion.Args.Internal (
  Arg,
  FunArgs (apply, type (~>)),
  HList,
  HandleArgs,
  IsLenient,
  IsRequired,
  RHList ((:#!)),
  RHListToHList (revHListToList),
  Reverse (reverseHList),
  RunDelayed (DelayedArgs, runDelayed),
  WithHeader (..),
  WithPiece (..),
  WithPieces (..),
  WithQueryParam (..),
  WithReq (..),
  type (:+),
 )
import Web.Minion.Error (
  ErrorBuilder,
  ErrorBuilders (..),
  NoMatch (..),
  ServerError,
 )
import Web.Minion.Introspect qualified as I
import Web.Minion.Request (IsRequest)
import Web.Minion.Response (CanRespond (..), ToResponse (..))

-- | If you don't care about introspection
type Router = Router' Void

type MiddlewareM m = ApplicationM m -> ApplicationM m

type MakeError = Http.Status -> Bytes.Lazy.ByteString -> ServerError

type ValueCombinator i v ts m = Router' i (ts :+ v) m -> Router' i ts m
type Combinator i ts m = Router' i ts m -> Router' i ts m

data MatchedPiece
  = StaticPiece {matchedPiece :: Text}
  | DynamicPiece {matchedPiece :: Text, placeholder :: Text}
  | DynamicPieces {matchedPieces :: [Text], placeholder :: Text}
  deriving (Eq, Ord, Show, Generic)

data MatchedHeader = MatchedHeader {headerName :: Http.HeaderName, headerValues :: [Bytes.ByteString]}
  deriving (Eq, Ord, Show, Generic)

data MatchedQuery = MatchedQuery {queryKey :: Bytes.ByteString, queryValues :: [Bytes.ByteString]}
  deriving (Eq, Ord, Show, Generic)

data MatchedData = MatchedData
  { path :: [MatchedPiece]
  , headers :: [MatchedHeader]
  , query :: [MatchedQuery]
  , method :: Http.Method
  }
  deriving (Eq, Ord, Show, Generic)

data Router' i (ts :: Type) m where
  Piece ::
    -- | .
    Text ->
    Router' i ts m ->
    Router' i ts m
  QueryParam ::
    forall a presence parsing m ts i.
    (I.Introspection i I.QueryParam a, IsRequired presence, IsLenient parsing) =>
    -- | Query param name
    ByteString ->
    -- | Parse query param
    (MakeError -> Maybe (NonEmpty (Maybe ByteString)) -> m (Arg presence parsing a)) ->
    Router' i (ts :+ WithQueryParam presence parsing m a) m ->
    Router' i ts m
  Captures ::
    forall a ts m i.
    (I.Introspection i I.Captures a) =>
    -- | Parse pieces
    (MakeError -> [Text] -> m [a]) ->
    -- | Placeholder
    Text ->
    Router' i (ts :+ WithPieces a) m ->
    Router' i ts m
  Capture ::
    forall a ts m i.
    (I.Introspection i I.Capture a) =>
    -- | Parse piece
    (MakeError -> Text -> m a) ->
    -- | Placeholder
    Text ->
    Router' i (ts :+ WithPiece a) m ->
    Router' i ts m
  Middleware ::
    MiddlewareM m ->
    Router' i ts m ->
    Router' i ts m
  Header ::
    forall a presence parsing m ts i.
    (I.Introspection i I.Header a, IsRequired presence, IsLenient parsing) =>
    Http.HeaderName ->
    -- | Parse header
    (MakeError -> [ByteString] -> m (Arg presence parsing a)) ->
    Router' i (ts :+ WithHeader presence parsing m a) m ->
    Router' i ts m
  Request ::
    forall r m i ts.
    (I.Introspection i I.Request r, IsRequest r) =>
    -- | .
    (ErrorBuilder -> Wai.Request -> m r) ->
    Router' i (ts :+ WithReq m r) m ->
    Router' i ts m
  Alt ::
    -- | Sub routes
    [Router' i ts m] ->
    Router' i ts m
  -- -- | Additional constraints provider with `request` and `response` can be useful for introspection
  Handle ::
    forall o m ts i st.
    ( HandleArgs ts st m
    , ToResponse m o
    , CanRespond o
    , I.Introspection i I.Response o
    ) =>
    -- | Handled HTTP method
    Http.Method ->
    (HList (DelayedArgs st) -> m o) ->
    Router' i ts m
  Description ::
    (I.Introspection i I.Description desc) =>
    -- | .
    desc ->
    Router' i ts m ->
    Router' i ts m
  MapArgs ::
    forall m ts ts' i.
    (RHList ts -> RHList ts') ->
    Router' i ts' m ->
    Router' i ts m
  HideIntrospection ::
    forall i' i ts m.
    Router' i ts m ->
    Router' i' ts m

{-# INLINE route #-}
route ::
  forall m ts i.
  (IO.MonadIO m, Exc.MonadCatch m) =>
  (forall a. MatchedData -> m a -> m a) ->
  ErrorBuilders ->
  RoutingState ->
  RHList ts ->
  Router' i ts m ->
  ApplicationM m
route withMatchedData ErrorBuilders{..} = go
 where
  {-# INLINE go #-}
  go :: forall ts' i'. (IO.MonadIO m, Exc.MonadCatch m) => RoutingState -> RHList ts' -> Router' i' ts' m -> ApplicationM m
  go state@RoutingState{..} args =
    \case
      Alt routes -> \req resp -> goThrough (NoMatch Nothing) $ map (\r -> go state args r req resp) routes
      Middleware mw r -> mw (go state args r)
      MapArgs f r -> go state (f args) r
      Description _ r -> go state args r
      HideIntrospection r -> go state args r
      Handle @o method f -> routeHandle withMatchedData state args method f
      Request @f get r -> \req resp -> go state (WithReq (get bodyErrorBuilder req) :#! args) r req resp
      Header @a @presence @parsing headerName get r -> \req ->
        let header = lookupHeader req headerName
            withHeader = WithHeader (get (headerErrorBuilder req) header) :#! args
         in go RoutingState{matchedHeaders = MatchedHeader headerName header : matchedHeaders, ..} withHeader r req
      QueryParam @a @presence @parsing queryParamName parse r -> \req ->
        let mbQueryParamVal = Nel.nonEmpty $ map snd $ filter ((queryParamName ==) . fst) $ Http.queryString req
            rawVals = map (fromMaybe "") $ Nel.toList $ fromMaybe [] mbQueryParamVal
            withQueryParam = WithQueryParam (parse (queryParamsErrorBuilder req) mbQueryParamVal) :#! args
         in go RoutingState{matchedQuery = MatchedQuery queryParamName rawVals : matchedQuery, ..} withQueryParam r req
      Piece txt r -> case path of
        (t : ts) | txt == t -> go RoutingState{path = ts, matchedPath = StaticPiece txt : matchedPath, ..} args r
        _ -> \_ _ -> throwMIO (NoMatch Nothing)
      Captures parse name r -> \req resp -> do
        parsed <- parse (captureErrorBuilder req) path
        go RoutingState{path = [], matchedPath = DynamicPieces path name : matchedPath, ..} (WithPieces parsed :#! args) r req resp
      Capture parse name r -> \req resp -> case path of
        (t : ts) -> do
          v <- parse (captureErrorBuilder req) t
          go RoutingState{path = ts, matchedPath = DynamicPiece t name : matchedPath, ..} (WithPiece v :#! args) r req resp
        _ -> throwMIO (NoMatch Nothing)

{-# INLINE routeHandle #-}
routeHandle ::
  forall m o ts st.
  (IO.MonadIO m, ToResponse m o, CanRespond o, HandleArgs ts st m) =>
  (forall a. MatchedData -> m a -> m a) ->
  RoutingState ->
  RHList ts ->
  Http.Method ->
  (HList (DelayedArgs st) -> m o) ->
  ApplicationM m
routeHandle withMatchedData RoutingState{..} args method f req resp = do
  checkHandler req path method
  let acceptHeader = lookupHeader req Http.hAccept
  if canRespond @o acceptHeader
    then do
      let matched = MatchedData{path = reverse matchedPath, headers = matchedHeaders, query = matchedQuery, method}
      args' <- runDelayed (reverseHList (revHListToList args))
      withMatchedData matched do
        f args' >>= (toResponse @m @o acceptHeader >=> IO.liftIO . resp)
    else IO.liftIO $ resp $ Wai.responseBuilder Http.status406 [] mempty

{-# INLINE goThrough #-}
goThrough :: (IO.MonadIO m, Exc.MonadCatch m) => NoMatch -> [m b] -> m b
goThrough _ (a : as) =
  Exc.try @_ @NoMatch a >>= \case
    Left e -> goThrough e as
    Right x -> pure x
goThrough e [] = throwMIO e

{-# INLINE throwMIO #-}
throwMIO :: (Exc.Exception e, IO.MonadIO m) => e -> m a
throwMIO = IO.liftIO . IOExc.throwIO

{-# INLINE checkHandler #-}
checkHandler :: (IO.MonadIO f) => Wai.Request -> [Text] -> Http.Method -> f ()
checkHandler req path method
  | method == Http.requestMethod req
  , null path || path == [mempty] =
      pure ()
  | otherwise = throwMIO (NoMatch Nothing)

{-# INLINE lookupHeader #-}
lookupHeader :: Wai.Request -> Http.HeaderName -> [ByteString]
lookupHeader req hn = map snd . filter ((hn ==) . fst) $ Http.requestHeaders req

instance IsList (Router' i ts r) where
  type Item (Router' i ts r) = Router' i ts r
  fromList = Alt . sortOn weight
   where
    weight :: Router' a b c -> Int
    weight = \case
      Capture{} -> 1
      Captures{} -> 1
      _ -> 0
  toList a = [a]

instance Semigroup (Router' i ts r) where
  a <> b = [a, b]

instance Monoid (Router' i ts r) where
  mempty = []

instance IsString (Combinator i ts m) where
  {-# INLINE fromString #-}
  fromString = smartPiece

{-# INLINE smartPiece #-}
smartPiece :: String -> Combinator i ts m
smartPiece (break (== '/') -> (a, as)) cont =
  if null as
    then Piece (fromString a) cont
    else Piece (fromString a) (smartPiece (drop 1 as) cont)

data RoutingState = RoutingState
  { path :: [Text]
  , matchedPath :: [MatchedPiece]
  , matchedQuery :: [MatchedQuery]
  , matchedHeaders :: [MatchedHeader]
  }

-- | 'Wai.Application' lifted to `m`
type ApplicationM m =
  Wai.Request ->
  ( Wai.Response ->
    IO Wai.ResponseReceived
  ) ->
  m Wai.ResponseReceived

{-# INLINE makeHandle #-}
makeHandle ::
  forall f o m ts i st.
  ( HandleArgs ts st m
  , ToResponse m (f o)
  , CanRespond (f o)
  , I.Introspection i I.Response (f o)
  ) =>
  Http.Method ->
  (o -> f o) ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
makeHandle method packResponse f =
  Handle @(f o) method (fmap packResponse . apply f)

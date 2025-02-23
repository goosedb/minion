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
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as Nel
import Data.Void (Void)
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
  ErrorBuilders ->
  RoutingState ->
  RHList ts ->
  Router' i ts m ->
  ApplicationM m
route builders state args (Alt routes) = \req resp -> goThrough $ map (\r -> route builders state args r req resp) routes
route builders state args (Middleware mw r) = mw (route builders state args r)
route builders state args (MapArgs f r) = route builders state (f args) r
route builders state args (Description _ r) = route builders state args r
route builders state args (HideIntrospection r) = route builders state args r
route _ RoutingState{..} args (Handle @o method f) = routeHandle path args method f
route builders@ErrorBuilders{..} state args (Request @f get r) = \req resp -> do
  route builders state (WithReq (get bodyErrorBuilder req) :#! args) r req resp
route builders@ErrorBuilders{..} state args (Header @a @presence @parsing headerName get r) = \req ->
  let header = lookupHeader req headerName
      withHeader = WithHeader (get (headerErrorBuilder req) header) :#! args
   in route builders state withHeader r req
route builders@ErrorBuilders{..} state args (QueryParam @a @presence @parsing queryParamName parse r) = \req ->
  let mbQueryParamVal = Nel.nonEmpty $ map snd $ filter ((queryParamName ==) . fst) $ Http.queryString req
      withQueryParam = WithQueryParam (parse (queryParamsErrorBuilder req) mbQueryParamVal) :#! args
   in route builders state withQueryParam r req
route builders RoutingState{..} args (Piece txt r) = case path of
  (t : ts) | txt == t -> route builders RoutingState{path = ts, ..} args r
  _ -> \_ _ -> throwMIO NoMatch
route builders@ErrorBuilders{..} RoutingState{..} args (Captures parse _ r) = \req resp -> do
  parsed <- parse (captureErrorBuilder req) path
  route builders RoutingState{path = [], ..} (WithPieces parsed :#! args) r req resp
route builders@ErrorBuilders{..} RoutingState{..} args (Capture parse _ r) = \req resp -> case path of
  (t : ts) -> do
    v <- parse (captureErrorBuilder req) t
    route builders RoutingState{path = ts, ..} (WithPiece v :#! args) r req resp
  _ -> throwMIO NoMatch

{-# INLINE routeHandle #-}
routeHandle ::
  forall m o ts st.
  (IO.MonadIO m, ToResponse m o, CanRespond o, HandleArgs ts st m) =>
  [Text] ->
  RHList ts ->
  Http.Method ->
  (HList (DelayedArgs st) -> m o) ->
  ApplicationM m
routeHandle path args method f req resp = do
  checkHandler req path method
  let acceptHeader = lookupHeader req Http.hAccept
  if canRespond @o acceptHeader
    then do
      args' <- runDelayed (reverseHList (revHListToList args))
      f args' >>= (toResponse @m @o acceptHeader >=> IO.liftIO . resp)
    else IO.liftIO $ resp $ Wai.responseBuilder Http.status406 [] mempty

{-# INLINE goThrough #-}
goThrough :: (IO.MonadIO m, Exc.MonadCatch m) => [m b] -> m b
goThrough (a : as) =
  Exc.try @_ @NoMatch a >>= \case
    Left NoMatch -> goThrough as
    Right x -> pure x
goThrough [] = throwMIO NoMatch

{-# INLINE throwMIO #-}
throwMIO :: (Exc.Exception e, IO.MonadIO m) => e -> m a
throwMIO = IO.liftIO . IOExc.throwIO

{-# INLINE checkHandler #-}
checkHandler :: (IO.MonadIO f) => Wai.Request -> [Text] -> Http.Method -> f ()
checkHandler req path method
  | method == Http.requestMethod req
  , null path || path == [mempty] =
      pure ()
  | otherwise = throwMIO NoMatch

{-# INLINE lookupHeader #-}
lookupHeader :: Wai.Request -> Http.HeaderName -> [ByteString]
lookupHeader req hn = map snd . filter ((hn ==) . fst) $ Http.requestHeaders req

instance IsList (Router' i ts r) where
  type Item (Router' i ts r) = Router' i ts r
  fromList = Alt
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

newtype RoutingState = RoutingState {path :: [Text]}

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

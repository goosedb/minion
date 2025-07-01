{-# LANGUAGE TupleSections #-}

module Web.Minion.Client (makeClient, runClient, defaultClientEnv, ClientApp, ClientEnv (..), ClientM (..), Endpoint (..)) where

import Control.Exception (catch)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Bool (bool)
import Data.ByteString qualified as Bytes
import Data.ByteString.Char8 qualified as Bytes.Char8
import Data.CaseInsensitive qualified as CI
import Data.Data (Proxy (..), tyConName)
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding as Text.Encoding
import Data.Traversable (for)
import Data.Typeable (tyConModule, tyConPackage)
import Data.Void (Void)
import Language.Haskell.TH.Syntax qualified as TH
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Type.Reflection qualified as Typeable
import Web.HttpApiData qualified as Http
import Web.Minion (IsRequired (isRequired), MonadThrow)
import Web.Minion.Client.Types
import Web.Minion.Introspect qualified as I
import Web.Minion.Router qualified as R

newtype Endpoint path a = Endpoint {client :: a}

type ClientApp = Wai.Request -> ClientM Wai.Response

data ClientEnv = ClientEnv
  { manager :: Http.Manager
  , baseUrl :: Text
  , middleware :: ClientApp -> ClientApp
  }

defaultClientEnv :: Text -> IO ClientEnv
defaultClientEnv baseUrl = do
  manager <- newManager defaultManagerSettings
  pure ClientEnv{middleware = id, ..}

newtype ClientM a = ClientM {runClientM :: ReaderT ClientEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadThrow)

runClient :: ClientEnv -> ClientM a -> IO a
runClient env ClientM{..} = runReaderT runClientM env

data EndpointParam
  = Piece Text
  | Capture Text TH.Type
  | Captures Text TH.Type
  | Query Bytes.ByteString Bool TH.Type
  | Header Http.HeaderName Bool TH.Type AnyHeaderClientValue TH.Type
  | Request TH.Type TH.Type

makeClient :: forall m i. (I.Elem Client i) => R.Router' i Void m -> TH.Q TH.Exp
makeClient = fmap (TH.TupE . map Just) . go [mempty]
 where
  wi :: forall t x a. (I.Introspection i t x) => ((I.IntrospectionFor Client t x) => a) -> a
  wi = I.withIntrospection @Client @i @t @x

  go :: [Seq EndpointParam] -> R.Router' i a m -> TH.Q [TH.Exp]
  go endpoints = \case
    R.Piece txt cont -> go (endpoints <&> (:|> Piece txt)) cont
    R.Capture @a _ txt cont -> wi @I.Capture @a do
      go (endpoints <&> (:|> Capture txt (globalNameOf @a))) cont
    R.Captures @a _ txt cont -> wi @I.Captures @a do
      go (endpoints <&> (:|> Captures txt (TH.AppT (TH.ConT ''[]) $ globalNameOf @a))) cont
    R.QueryParam @a @presence qn _ cont -> wi @I.QueryParam @a do
      let queryType = bool (TH.AppT (TH.ConT ''Maybe) $ globalNameOf @a) (globalNameOf @a) (isRequired @presence)
      go (endpoints <&> (:|> Query qn (isRequired @presence) queryType)) cont
    R.Description _ cont -> go endpoints cont
    R.Middleware _ cont -> go endpoints cont
    R.Header @a @presence hn _ cont -> wi @I.Header @a do
      go (endpoints <&> (:|> Header hn (isRequired @presence) (globalNameOf @a) (AnyHeaderClientValue $ headerValue @a) (globalNameOf @(HeaderFromClient a)))) cont
    R.Request @r _ cont -> wi @I.Request @r do
      go (endpoints <&> (:|> Request (globalNameOf @r) (globalNameOf @(RequestFromClient r)))) cont
    R.Alt alts -> concat <$> traverse (go endpoints) alts
    R.Handle @o method _ -> for endpoints \(toList -> params) -> wi @I.Response @o do
      namedParams <- nameAllParams params
      let lambdaParams = generateLambdaParams namedParams
      constantValue <- generateConstantParams namedParams
      let addConstantValues = if null constantValue then id else TH.LetE constantValue
      path <- generatePath namedParams
      let url = typePath method (map snd namedParams)
      let ep = TH.AppTypeE (TH.ConE 'Endpoint) (TH.LitT $ TH.StrTyLit url)
      queries <- generateQueries namedParams
      headers <- generateHeaders namedParams
      attachBody <- generateBody namedParams
      let responseType = globalNameOf @o
      readResponse <- [|$(pure $ TH.AppTypeE (TH.VarE 'checkAndDecode) responseType)|]
      TH.AppE ep . TH.LamE lambdaParams . addConstantValues
        <$> [|
          ClientM do
            ClientEnv manager baseUrl _ <- ask
            rawRequest <- Http.parseUrlThrow $ Text.unpack (baseUrl <> $(pure path))
            let requestWithQuery = Http.setQueryString $(pure queries) rawRequest
            let requestWithHeaders = requestWithQuery{Http.requestHeaders = $(pure headers), Http.method = method}
            let requestWithBody = $(pure attachBody) requestWithHeaders
            request <- $(pure $ TH.VarE 'liftIO) requestWithBody
            liftIO $ Http.responseOpen request manager >>= $(pure readResponse)
          |]
    R.MapArgs _ cont -> go endpoints cont
    R.HideIntrospection _ -> pure []
    R.Raw _ -> pure []

generateBody :: [(TH.Name, EndpointParam)] -> TH.Q TH.Exp
generateBody =
  fmap (fromMaybe (TH.VarE 'pure) . listToMaybe) . sequence . mapMaybe
    \case
      (_, Request _ typ) | typ == TH.ConT ''Void -> Nothing
      (name, Request typ _) -> Just [|$(pure $ TH.AppTypeE (TH.VarE 'packRequest) typ) $(pure $ TH.VarE name)|]
      _ -> Nothing

nameAllParams :: [EndpointParam] -> TH.Q [(TH.Name, EndpointParam)]
nameAllParams params = for params \a ->
  let newName name = TH.newName name <&> (,a)
   in case a of
        Piece{} -> newName "piece"
        Capture{} -> newName "capture"
        Captures{} -> newName "captures"
        Query{} -> newName "captures"
        Header{} -> newName "header"
        Request{} -> newName "body"

generateLambdaParams :: [(TH.Name, EndpointParam)] -> [TH.Pat]
generateLambdaParams = mapMaybe
  \case
    (_, Piece _) -> Nothing
    (name, Capture _ typ) -> pat name typ
    (name, Captures _ typ) -> pat name typ
    (name, Query _ _ typ) -> pat name typ
    (_, Header _ _ _ _ typ) | typ == TH.ConT ''Void -> Nothing
    (name, Header _ _ _ (AnyHeaderClientValue val) typ) -> case val of
      ConstantHeaderValue _ -> Nothing
      NoHeaderValue -> Nothing
      PassedHeaderValue -> pat name typ
    (name, Request _ typ) -> pat name typ
 where
  pat name typ = Just $ TH.SigP (TH.VarP name) typ

generateConstantParams :: [(TH.Name, EndpointParam)] -> TH.Q [TH.Dec]
generateConstantParams =
  fmap catMaybes . mapM
    \case
      (_, Header _ _ _ _ typ) | typ == TH.ConT ''Void -> pure Nothing
      (name, Header _ _ apiTyp (AnyHeaderClientValue val) _) -> case val of
        ConstantHeaderValue{} -> do
          passedValue <-
            [|
              case $(pure $ TH.AppTypeE (TH.VarE 'headerValue) apiTyp) of
                ConstantHeaderValue v -> v
                _ -> error "impossible!"
              |]
          pure $ Just $ TH.ValD (TH.VarP name) (TH.NormalB passedValue) []
        NoHeaderValue -> pure Nothing
        PassedHeaderValue -> pure Nothing
      _ -> pure Nothing

generatePath :: [(TH.Name, EndpointParam)] -> TH.Q TH.Exp
generatePath =
  fmap (intercalateText . assemblePath . collectPath (Right []) . catMaybes) . traverse @_ @TH.Q
    \case
      (_, Piece p) -> Just . Right <$> [|p|]
      (name :: TH.Name, Capture _ _) -> Just . Right <$> [|Http.toUrlPiece $(pure $ TH.VarE name)|]
      (name, Captures _ _) -> Just . Left <$> [|Http.toUrlPiece <$> $(pure $ TH.VarE name)|]
      _ -> pure Nothing
 where
  collectPath :: Either [TH.Exp] [TH.Exp] -> [Either TH.Exp TH.Exp] -> [TH.Exp]
  collectPath (Right as) (Right a : rest) = collectPath (Right (a : as)) rest
  collectPath (Right as) (Left a : rest) = TH.ListE (reverse as) : collectPath (Left [a]) rest
  collectPath (Left as) (Left a : rest) = collectPath (Left (a : as)) rest
  collectPath (Left as) (Right a : rest) = as <> collectPath (Left [a]) rest
  collectPath (Right as) [] = [TH.ListE (reverse as)]
  collectPath (Left as) [] = reverse as

  assemblePath [] = TH.ListE []
  assemblePath as = foldl1 (\a b -> TH.InfixE (Just a) (TH.VarE '(<>)) (Just b)) as

  intercalateText = TH.AppE do
    TH.InfixE
      do Just (TH.VarE 'Text.concat)
      do TH.VarE '(.)
      do Just $ TH.AppE (TH.VarE 'map) (TH.InfixE (Just $ TH.LitE $ TH.StringL "/") (TH.VarE '(<>)) Nothing)

typePath :: Bytes.Char8.ByteString -> [EndpointParam] -> [Char]
typePath method namedParams =
  Bytes.Char8.unpack method
    <> " "
    <> Text.unpack
      ( Text.concat . map ("/" <>) $ mapMaybe
          do
            \case
              Piece n -> Just n
              Capture n _ -> Just ("{" <> n <> "}")
              Captures n _ -> Just ("{" <> n <> "..}")
              _ -> Nothing
          do namedParams
      )

generateQueries :: [(TH.Name, EndpointParam)] -> TH.Q TH.Exp
generateQueries =
  fmap TH.ListE . sequence . mapMaybe
    \case
      (name, Query qn isQueryRequired _) -> Just do
        liftedQn <- TH.lift qn
        let packRequired = if isQueryRequired then TH.AppE (TH.VarE 'Just) else id
        pure $ TH.TupE $ map Just [liftedQn, packRequired (TH.AppE (TH.VarE 'packQuery) (TH.VarE name))]
      _ -> Nothing

generateHeaders :: [(TH.Name, EndpointParam)] -> TH.Q TH.Exp
generateHeaders =
  fmap TH.ListE . sequence . mapMaybe
    \case
      (_, Header _ _ _ _ typ) | typ == TH.ConT ''Void -> Nothing
      (name, Header (CI.original -> hn) isHeaderRequired _ _ _) -> Just do
        liftedHn <- TH.AppE (TH.VarE 'CI.mk) <$> TH.lift hn
        let toHeader = TH.VarE 'Http.toHeader
        let packRequired = if isHeaderRequired then TH.AppE toHeader else TH.InfixE (Just toHeader) (TH.VarE '(<$>)) . Just
        pure $ TH.TupE $ map Just [liftedHn, packRequired (TH.VarE name)]
      _ -> Nothing

packQuery :: (Http.ToHttpApiData q) => q -> Bytes.Char8.ByteString
packQuery = Text.Encoding.encodeUtf8 . Http.toQueryParam

globalNameOf :: forall a. (Typeable.Typeable a) => TH.Type
globalNameOf = case Typeable.someTypeRep (Proxy @a) of
  Typeable.SomeTypeRep a -> globalName a

globalName :: Typeable.TypeRep x -> TH.Type
globalName (Typeable.Con tr) = case n of
  "'[]" -> TH.PromotedNilT
  "':" -> TH.PromotedConsT
  '\'' : '(' : tuple | last tuple == ')' -> TH.PromotedTupleT $ 1 + length (filter (== ',') tuple)
  _ -> TH.ConT $ TH.Name (TH.OccName n) (TH.NameG TH.TcClsName (TH.PkgName p) (TH.ModName m))
 where
  m = tyConModule tr
  p = tyConPackage tr
  n = tyConName tr
globalName (Typeable.App a b) = TH.AppT (globalName a) (globalName b)

checkAndDecode :: forall v. (ResponseClient v) => Http.Response Http.BodyReader -> IO (Either ClientError (ResponseForClient v))
checkAndDecode resp = acceptResponse @v resp `catch` (pure . Left . ConnectionError)

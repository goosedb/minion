{-# LANGUAGE InstanceSigs #-}

module Web.Minion.Request.Query.QueryParams (QueryParams (..), QueryParamsStrict (..)) where

import Control.Monad.Catch (MonadThrow (throwM))
import Data.ByteString qualified as Bytes
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as Nel
import Web.HttpApiData (FromHttpApiData)
import Web.Minion.Args.Internal
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Query.Internal
import Web.Minion.Router.Internal

newtype QueryParams a = QueryParams (Nel.NonEmpty a)

class QueryParamsStrict presence where
  queryParams ::
    forall a m i ts.
    (FromHttpApiData a, I.Introspection i I.QueryParam (QueryParams a), MonadThrow m) =>
    -- | .
    QueryParamName ->
    ValueCombinator i (WithQueryParam presence Strict m (Nel.NonEmpty a)) ts m

instance QueryParamsStrict Optional where
  queryParams ::
    forall a m i ts.
    (FromHttpApiData a, I.Introspection i I.QueryParam (QueryParams a), MonadThrow m) =>
    -- \| .
    QueryParamName ->
    ValueCombinator i (WithQueryParam Optional Strict m (Nel.NonEmpty a)) ts m
  queryParams qn cont = withQueryParam @(QueryParams a) @Optional @Strict
    qn
    do
      \makeError ->
        maybe (pure Nothing) $
          maybe (badReq makeError noValueError qn) (fmap Just . parseQueryParams @a @m makeError) . filterNothings
    do MapArgs (\(WithQueryParam t :#! ts) -> WithQueryParam (fmap coerce <$> t) :#! ts) cont

instance QueryParamsStrict Required where
  queryParams ::
    forall a m i ts.
    (FromHttpApiData a, I.Introspection i I.QueryParam (QueryParams a), MonadThrow m) =>
    -- \| .
    QueryParamName ->
    ValueCombinator i (WithQueryParam Required Strict m (Nel.NonEmpty a)) ts m
  queryParams qn cont = withQueryParam @(QueryParams a) @Required @Strict
    qn
    do
      \makeError ->
        maybe
          do badReq makeError noKeyError qn
          do maybe (badReq makeError noValueError qn) (parseQueryParams @a makeError) . filterNothings
    do MapArgs (\(WithQueryParam t :#! ts) -> WithQueryParam (coerce <$> t) :#! ts) cont

parseQueryParams :: forall a m. (FromHttpApiData a, MonadThrow m) => MakeError -> Nel.NonEmpty Bytes.ByteString -> m (QueryParams a)
parseQueryParams makeError = either throwM (pure . QueryParams) . traverse (tryDecode @a makeError)

module Web.Minion.Request.Query.Param (
  QueryParamStrict (..),
  QueryParamLenient (..),
) where

import Control.Applicative (asum)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Text (Text)
import Web.HttpApiData (FromHttpApiData)
import Web.Minion.Args.Internal
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Query.Internal
import Web.Minion.Router.Internal

class QueryParamStrict presence where
  queryParam ::
    forall a m i ts.
    (FromHttpApiData a, I.Introspection i I.QueryParam a, MonadThrow m) =>
    QueryParamName ->
    ValueCombinator i (WithQueryParam presence Strict m a) ts m

instance QueryParamStrict Required where
  queryParam qn = withQueryParam qn \makeError -> maybe
    do badReq makeError noKeyError qn
    do maybe (badReq makeError noValueError qn) (either throwM pure . tryDecode makeError) . asum

instance QueryParamStrict Optional where
  queryParam qn = withQueryParam qn \makeError ->
    maybe (pure Nothing) (fmap Just . either throwM pure . tryDecode makeError) . (asum =<<)

class QueryParamLenient presence where
  queryParamLenient ::
    forall a m i ts.
    (FromHttpApiData a, I.Introspection i I.QueryParam a, MonadThrow m) =>
    QueryParamName ->
    ValueCombinator i (WithQueryParam presence (Lenient Text) m a) ts m

instance QueryParamLenient Optional where
  queryParamLenient qn = withQueryParam qn \_ ->
    maybe (pure Nothing) (pure . Just . decodeQueryParam) . (asum =<<)

instance QueryParamLenient Required where
  queryParamLenient qn = withQueryParam qn \makeError ->
    maybe
      (badReq makeError noKeyError qn)
      (maybe (badReq makeError noValueError qn) (pure . decodeQueryParam) . asum)

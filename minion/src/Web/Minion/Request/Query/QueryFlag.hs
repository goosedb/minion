module Web.Minion.Request.Query.QueryFlag (QueryFlag (..), QueryFlagStrict (..)) where

import Control.Applicative (asum)
import Control.Monad.Catch (MonadThrow)
import Data.ByteString qualified as Bytes
import Web.Minion.Args.Internal
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Query.Internal
import Web.Minion.Router.Internal

newtype QueryFlag = QueryFlag Bool

class QueryFlagStrict presence where
  queryFlag ::
    forall m i ts.
    (I.Introspection i I.QueryParam QueryFlag, MonadThrow m) =>
    -- | .
    QueryParamName ->
    ValueCombinator i (WithQueryParam presence Strict m QueryFlag) ts m

instance QueryFlagStrict Required where
  queryFlag qn = withQueryParam qn \makeError ->
    maybe
      (badReq makeError noKeyError qn)
      (maybe (pure (QueryFlag True)) (pure . parseQueryFlag) . asum)

instance QueryFlagStrict Optional where
  queryFlag qn = withQueryParam qn \_ ->
    maybe
      (pure Nothing)
      (fmap Just . maybe (pure (QueryFlag True)) (pure . parseQueryFlag) . asum)

parseQueryFlag :: Bytes.ByteString -> QueryFlag
parseQueryFlag = QueryFlag . flip (elem @[]) ["1", "true", ""]

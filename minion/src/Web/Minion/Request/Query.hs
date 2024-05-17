module Web.Minion.Request.Query (
  QueryForm (..),
  QueryFlag (..),
  QueryParamName,
  queryParamsForm,
  queryFlag,
  queryFlag',
  queryParam,
  queryParam',
  queryParamLenient,
  queryParamLenient',
) where

import Control.Monad (join, (>=>))
import Control.Monad.Catch (MonadThrow (throwM))
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.String.Conversions (ConvertibleStrings (convertString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encode
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Http
import Web.FormUrlEncoded (FromForm)
import Web.FormUrlEncoded qualified as Http
import Web.HttpApiData (FromHttpApiData)
import Web.Minion.Args.Internal
import Web.Minion.Introspect qualified as I
import Web.Minion.Request
import Web.Minion.Request.Query.Internal
import Web.Minion.Router.Internal

type QueryParamName = Text.Text

{- | Tries to get query param

@
'queryParam'' "foo" pure '/>' ...
@
-}
{-# INLINE queryParam' #-}
queryParam' ::
  forall a m i ts.
  (FromHttpApiData a, I.Introspection i I.QueryParam a, MonadThrow m) =>
  -- | .
  QueryParamName ->
  ValueCombinator i (WithQueryParam Required Strict m a) ts m
queryParam' qn = QueryParam (Text.Encode.encodeUtf8 qn) \makeError ->
  let badReq err = throwM $ makeError Http.status400 $ err qn
   in maybe
        do badReq queryParamKeyNotFoundError
        do
          maybe
            (badReq queryParamValueNotFoundError)
            (either throwM pure . decodeQueryParamOrServerError makeError)

{- | Tries to get query param

@
'queryParam' "foo" pure '/>' ...
@
-}
{-# INLINE queryParam #-}
queryParam ::
  forall a m i ts.
  (FromHttpApiData a, I.Introspection i I.QueryParam a, MonadThrow m) =>
  -- | .
  QueryParamName ->
  ValueCombinator i (WithQueryParam Optional Strict m a) ts m
queryParam qn = QueryParam (Text.Encode.encodeUtf8 qn) \makeError ->
  maybe
    (pure Nothing)
    (fmap Just . either throwM pure . decodeQueryParamOrServerError @a makeError)
    . join

{- | Tries to get query param

@
'queryParam' "foo" pure '/>' ...
@
-}
{-# INLINE queryParamLenient #-}
queryParamLenient ::
  forall a m i ts.
  (FromHttpApiData a, I.Introspection i I.QueryParam a, Monad m) =>
  -- | .
  QueryParamName ->
  ValueCombinator i (WithQueryParam Optional (Lenient Text) m a) ts m
queryParamLenient qn = QueryParam (Text.Encode.encodeUtf8 qn) \_ ->
  maybe
    (pure Nothing)
    (pure . Just . decodeQueryParam)
    . join

{- | Tries to get query param

@
'queryParam' "foo" pure '/>' ...
@
-}
{-# INLINE queryParamLenient' #-}
queryParamLenient' ::
  forall a m i ts.
  (FromHttpApiData a, I.Introspection i I.QueryParam a, MonadThrow m) =>
  -- | .
  QueryParamName ->
  ValueCombinator i (WithQueryParam Required (Lenient Text) m a) ts m
queryParamLenient' qn = QueryParam (Text.Encode.encodeUtf8 qn) \makeError ->
  let badReq err = throwM $ makeError Http.status400 $ err qn
   in maybe
        (badReq queryParamKeyNotFoundError)
        (maybe (badReq queryParamValueNotFoundError) $ pure . decodeQueryParam)

{- | Extracts query string to `Form`

@
... '/>' 'queryParamsForm' \@MyForm '.>' ...
@
-}
{-# INLINE queryParamsForm #-}
queryParamsForm ::
  forall r m i ts.
  (I.Introspection i I.Request (QueryForm r), MonadThrow m, FromForm r) =>
  -- | .
  ValueCombinator i (WithReq m (QueryForm r)) ts m
queryParamsForm = Request \makeError req ->
  either (throwM . makeError req Http.status400 . convertString) (pure . QueryForm)
    . (Http.urlDecodeForm >=> Http.fromForm)
    . Bytes.Lazy.fromStrict
    . Http.rawQueryString
    $ req

newtype QueryFlag = QueryFlag Bool

{-# INLINE queryFlag' #-}
queryFlag' ::
  forall m i ts.
  (I.Introspection i I.QueryParam QueryFlag, MonadThrow m) =>
  -- | .
  QueryParamName ->
  ValueCombinator i (WithQueryParam Required Strict m QueryFlag) ts m
queryFlag' qn = QueryParam (Text.Encode.encodeUtf8 qn) \makeError ->
  maybe
    (throwM $ makeError Http.status400 $ queryParamKeyNotFoundError qn)
    (maybe (pure (QueryFlag True)) $ pure . parseQueryFlag)

{-# INLINE queryFlag #-}
queryFlag ::
  forall m i ts.
  (I.Introspection i I.QueryParam QueryFlag, Monad m) =>
  -- | .
  QueryParamName ->
  ValueCombinator i (WithQueryParam Optional Strict m QueryFlag) ts m
queryFlag qn = QueryParam (Text.Encode.encodeUtf8 qn) \_ ->
  maybe
    (pure Nothing)
    (fmap Just . maybe (pure (QueryFlag True)) (pure . parseQueryFlag))

parseQueryFlag :: Bytes.ByteString -> QueryFlag
parseQueryFlag = QueryFlag . flip (elem @[]) ["1", "true", ""]

newtype QueryForm a = QueryForm a

instance IsRequest (QueryForm a) where
  type RequestValue (QueryForm a) = a
  getRequestValue (QueryForm a) = a

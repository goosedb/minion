module Web.Minion.Request.Query.Form (QueryForm (..), queryParamsForm) where

import Control.Monad.Catch
import Data.Bifunctor (Bifunctor (..))
import Data.String.Conversions (ConvertibleStrings (..))
import Data.Text.Encoding qualified as Text
import GHC.IsList
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Http
import Web.FormUrlEncoded
import Web.Internal.FormUrlEncoded qualified as Http
import Web.Minion.Args.Internal
import Web.Minion.Introspect qualified as I
import Web.Minion.Request
import Web.Minion.Router.Internal

newtype QueryForm a = QueryForm a

instance IsRequest (QueryForm a) where
  type RequestValue (QueryForm a) = a
  getRequestValue (QueryForm a) = a

{-# INLINE queryParamsForm #-}
queryParamsForm ::
  forall r m i ts.
  (I.Introspection i I.Request (QueryForm r), MonadThrow m, FromForm r) =>
  -- | .
  ValueCombinator i (WithReq m (QueryForm r)) ts m
queryParamsForm = Request \makeError req ->
  either (throwM . makeError req Http.status400 . convertString) (pure . QueryForm)
    . Http.fromForm
    . fromList
    . fmap (bimap Text.decodeUtf8 $ maybe mempty Text.decodeUtf8)
    . Http.queryString
    $ req

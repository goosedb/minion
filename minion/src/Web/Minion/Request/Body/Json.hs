module Web.Minion.Request.Body.Json where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import Web.Minion.Args (WithReq)
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.Json (Json)
import Web.Minion.Request.Body (
  ReqBody,
  reqBody,
 )
import Web.Minion.Router (ValueCombinator)

{- | Extracts JSON from request

@
... '/>' 'reqJson' @MyType '.>' ...
@
-}
reqJson ::
  forall r m i ts.
  (I.Introspection i I.Request (ReqBody '[Json] r)) =>
  (FromJSON r) =>
  (MonadIO m, MonadThrow m) =>
  -- | .
  ValueCombinator i (WithReq m (ReqBody '[Json] r)) ts m
reqJson = reqBody

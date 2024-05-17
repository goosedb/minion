module Web.Minion.Request.Body.FormUrlEncoded where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Web.Minion.Args
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.FormUrlEncoded
import Web.Minion.Request.Body
import Web.Minion.Router (ValueCombinator)

reqFormUrlEncoded ::
  forall r m i ts.
  (I.Introspection i I.Request (ReqBody '[FormUrlEncoded] r)) =>
  (MonadIO m, MonadThrow m) =>
  (Decode FormUrlEncoded r) =>
  -- | .
  ValueCombinator i (WithReq m (ReqBody '[FormUrlEncoded] r)) ts m
reqFormUrlEncoded = reqBody

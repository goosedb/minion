module Web.Minion.Request.Body.PlainText where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Web.Minion.Args
import Web.Minion.Introspect qualified as I
import Web.Minion.Media.PlainText
import Web.Minion.Request.Body
import Web.Minion.Router (ValueCombinator)

reqPlainText ::
  forall r m i ts.
  (I.Introspection i I.Request (ReqBody '[PlainText] r)) =>
  (MonadIO m, MonadThrow m) =>
  (Decode PlainText r) =>
  -- | .
  ValueCombinator i (WithReq m (ReqBody '[PlainText] r)) ts m
reqPlainText = reqBody

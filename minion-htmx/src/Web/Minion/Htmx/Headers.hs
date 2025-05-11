module Web.Minion.Htmx.Headers where

import Data.Bool (bool)
import Data.List.NonEmpty qualified as Nel
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encode
import Web.Minion.Args
import Web.Minion.Error
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Header
import Web.Minion.Router (Router' (..), ValueCombinator)

-- | Matches only `HX-Request:true`. Otherwise throws 'NoMatch' that causes trying another route
hxRequest :: (MonadThrow m, I.Introspection i I.Header Bool) => ValueCombinator i (WithHeader Required Strict m Bool) ts m
hxRequest = Header @Bool @Required @Strict "HX-Request" \_ -> bool (throwM $ NoMatch Nothing) (pure True) . ("true" `elem`)

hxTarget ::
  forall presence m i ts.
  (I.Introspection i I.Header Text, MonadThrow m, HeaderStrict presence) =>
  ValueCombinator i (WithHeader presence Strict m Text) ts m
hxTarget = header @presence "HX-Target" (const $ pure . Text.Encode.decodeUtf8 . Nel.head)

hxTrigger ::
  forall presence m i ts.
  (I.Introspection i I.Header Text, MonadThrow m, HeaderStrict presence) =>
  ValueCombinator i (WithHeader presence Strict m Text) ts m
hxTrigger = header @presence "HX-Trigger" (const $ pure . Text.Encode.decodeUtf8 . Nel.head)

hxTriggerName ::
  forall presence m i ts.
  (I.Introspection i I.Header Text, MonadThrow m, HeaderStrict presence) =>
  ValueCombinator i (WithHeader presence Strict m Text) ts m
hxTriggerName = header @presence "HX-Trigger-Name" (const $ pure . Text.Encode.decodeUtf8 . Nel.head)

hxCurrentUrl' ::
  forall presence m i ts.
  (I.Introspection i I.Header Text, MonadThrow m, HeaderStrict presence) =>
  ValueCombinator i (WithHeader presence Strict m Text) ts m
hxCurrentUrl' = header @presence "HX-Current-URL" (const $ pure . Text.Encode.decodeUtf8 . Nel.head)

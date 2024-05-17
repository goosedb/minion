module Web.Minion.Htmx.Headers where

import Data.Bool (bool)
import Data.List.NonEmpty qualified as Nel
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encode
import Web.Minion.Args
import Web.Minion.Error
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Header
import Web.Minion.Router (Router' (..), ValueCombinator)

-- | Matches only `HX-Request:true`. Otherwise throws 'NoMatch' that causes trying another route
hxRequest :: (MonadThrow m, I.Introspection i I.Header Bool) => ValueCombinator i (WithHeader Required Strict m Bool) ts m
hxRequest = Header @Bool @Required @Strict "HX-Request" \_ -> bool (throwM NoMatch) (pure True) . ("true" `elem`)

hxTarget ::
  (I.Introspection i I.Header Text, MonadThrow m) =>
  ValueCombinator i (WithHeader Optional Strict m Text) ts m
hxTarget = header "HX-Target" (const $ pure . fmap Text.Encode.decodeUtf8 . listToMaybe)

hxTarget' ::
  (I.Introspection i I.Header Text, MonadThrow m) =>
  ValueCombinator i (WithHeader Required Strict m Text) ts m
hxTarget' = header' "HX-Target" (const $ pure . Text.Encode.decodeUtf8 . Nel.head)

hxTrigger ::
  (I.Introspection i I.Header Text, MonadThrow m) =>
  ValueCombinator i (WithHeader Optional Strict m Text) ts m
hxTrigger = header "HX-Trigger" (const $ pure . fmap Text.Encode.decodeUtf8 . listToMaybe)

hxTrigger' ::
  (I.Introspection i I.Header Text, MonadThrow m) =>
  ValueCombinator i (WithHeader Required Strict m Text) ts m
hxTrigger' = header' "HX-Trigger" (const $ pure . Text.Encode.decodeUtf8 . Nel.head)

hxTriggerName ::
  (I.Introspection i I.Header Text, MonadThrow m) =>
  ValueCombinator i (WithHeader Optional Strict m Text) ts m
hxTriggerName = header "HX-Trigger-Name" (const $ pure . fmap Text.Encode.decodeUtf8 . listToMaybe)

hxTriggerName' ::
  (I.Introspection i I.Header Text, MonadThrow m) =>
  ValueCombinator i (WithHeader Required Strict m Text) ts m
hxTriggerName' = header' "HX-Trigger-Name" (const $ pure . Text.Encode.decodeUtf8 . Nel.head)

hxCurrentUrl ::
  (I.Introspection i I.Header Text, MonadThrow m) =>
  ValueCombinator i (WithHeader Optional Strict m Text) ts m
hxCurrentUrl = header "HX-Current-URL" (const $ pure . fmap Text.Encode.decodeUtf8 . listToMaybe)

hxCurrentUrl' ::
  (I.Introspection i I.Header Text, MonadThrow m) =>
  ValueCombinator i (WithHeader Required Strict m Text) ts m
hxCurrentUrl' = header' "HX-Current-URL" (const $ pure . Text.Encode.decodeUtf8 . Nel.head)

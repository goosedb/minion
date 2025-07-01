module Web.Minion.Htmx.Headers where

import Data.Bool (bool)
import Data.List.NonEmpty qualified as Nel
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encode
import Web.HttpApiData qualified as Http
import Web.Minion ((!>))
import Web.Minion.Args
import Web.Minion.Client.Types
import Web.Minion.Error
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Header
import Web.Minion.Router (ValueCombinator)

data HxRequest = HxRequest

instance HeaderClient HxRequest where
  type HeaderFromClient HxRequest = Bool
  headerValue = ConstantHeaderValue True

instance HeaderClient HxTrigger where
  type HeaderFromClient HxTrigger = HxTrigger
  headerValue = PassedHeaderValue

instance HeaderClient HxTriggerName where
  type HeaderFromClient HxTriggerName = HxTriggerName
  headerValue = PassedHeaderValue

instance HeaderClient HxCurrentUrl where
  type HeaderFromClient HxCurrentUrl = HxCurrentUrl
  headerValue = PassedHeaderValue

newtype HxTarget = HxTarget {getHxTarget :: Text}
  deriving newtype (Http.FromHttpApiData, Http.ToHttpApiData)

newtype HxTrigger = HxTrigger {getHxTrigger :: Text}
  deriving newtype (Http.FromHttpApiData, Http.ToHttpApiData)

newtype HxTriggerName = HxTriggerName {getHxTriggerName :: Text}
  deriving newtype (Http.FromHttpApiData, Http.ToHttpApiData)

newtype HxCurrentUrl = HxCurrentUrl {getHxCurrentUrl :: Text}
  deriving newtype (Http.FromHttpApiData, Http.ToHttpApiData)

-- | Matches only `HX-Request:true`. Otherwise throws 'NoMatch' that causes trying another route
hxRequest :: (MonadThrow m, I.Introspection i I.Header HxRequest) => ValueCombinator i (Hide (WithHeader Required Strict m HxRequest)) ts m
hxRequest next = header @Required "HX-Request" (\_ -> bool (throwM $ NoMatch Nothing) (pure HxRequest) . ("true" `elem`)) !> next

hxTarget ::
  forall presence m i ts.
  (I.Introspection i I.Header HxTarget, MonadThrow m, HeaderStrict presence) =>
  ValueCombinator i (WithHeader presence Strict m HxTarget) ts m
hxTarget = header @presence "HX-Target" (const $ pure . HxTarget . Text.Encode.decodeUtf8 . Nel.head)

hxTrigger ::
  forall presence m i ts.
  (I.Introspection i I.Header HxTrigger, MonadThrow m, HeaderStrict presence) =>
  ValueCombinator i (WithHeader presence Strict m HxTrigger) ts m
hxTrigger = header @presence "HX-Trigger" (const $ pure . HxTrigger . Text.Encode.decodeUtf8 . Nel.head)

hxTriggerName ::
  forall presence m i ts.
  (I.Introspection i I.Header HxTriggerName, MonadThrow m, HeaderStrict presence) =>
  ValueCombinator i (WithHeader presence Strict m HxTriggerName) ts m
hxTriggerName = header @presence "HX-Trigger-Name" (const $ pure . HxTriggerName . Text.Encode.decodeUtf8 . Nel.head)

hxCurrentUrl' ::
  forall presence m i ts.
  (I.Introspection i I.Header HxCurrentUrl, MonadThrow m, HeaderStrict presence) =>
  ValueCombinator i (WithHeader presence Strict m HxCurrentUrl) ts m
hxCurrentUrl' = header @presence "HX-Current-URL" (const $ pure . HxCurrentUrl . Text.Encode.decodeUtf8 . Nel.head)

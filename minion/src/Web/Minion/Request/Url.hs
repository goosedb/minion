module Web.Minion.Request.Url (capture, captures, piece) where

import Control.Monad.Catch (MonadThrow (throwM))
import Data.Bifunctor (Bifunctor (..))
import Data.String (IsString (..))
import Data.String.Conversions (ConvertibleStrings (..))
import Data.Text (Text)
import Network.HTTP.Types qualified as Http
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), parseUrlPieces)
import Web.Minion.Args.Internal
import Web.Minion.Error (NoMatch (NoMatch))
import Web.Minion.Introspect qualified as I
import Web.Minion.Router.Internal

{- | Captures one piece of path

@
'capture' \@Text '.>' ...
@
-}
{-# INLINE capture #-}
capture ::
  forall b m i ts.
  (FromHttpApiData b, I.Introspection i I.Capture b, MonadThrow m) =>
  -- } .
  Text ->
  ValueCombinator i (WithPiece b) ts m
capture =
  Capture @b
    ( \makeError ->
        either throwM pure
          . first (NoMatch . Just . makeError Http.status400 . convertString)
          . parseUrlPiece
    )

{- | Captures the rest of path

@
'captures' \@Text '.>' ...
@
-}
{-# INLINE captures #-}
captures ::
  forall b m i ts.
  (FromHttpApiData b, I.Introspection i I.Captures b, MonadThrow m) =>
  -- | .
  Text ->
  ValueCombinator i (WithPieces b) ts m
captures =
  Captures @b
    \makeError ->
      either throwM pure
        . first (NoMatch . Just . makeError Http.status400 . convertString)
        . parseUrlPieces

{- | Could be omitted with `OverloadedStrings`

@
{\-# LANGUAGE OverloadedStrings #-\}
"bar" '/>' ...
@

@
{\-# LANGUAGE NoOverloadedStrings #-\}
'piece' "bar" '/>' ...
@

Also splits piece with /, so

@
piece "foo\/bar\/bar" == piece "foo" '/>' piece "bar" '/>' piece "baz"
"foo\/bar\/baz" == "foo" '/>' "bar" '/>' "baz"
@
-}
{-# INLINE piece #-}
piece :: String -> Combinator i ts m
piece = fromString

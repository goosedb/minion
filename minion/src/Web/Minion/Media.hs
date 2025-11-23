module Web.Minion.Media (ContentType (..), AllContentTypes (..)) where

import Data.Kind (Type)
import Data.List.NonEmpty qualified as Nel
import Network.HTTP.Media qualified as Http

class ContentType a where
  media :: Nel.NonEmpty Http.MediaType

class AllContentTypes (cts :: [Type]) where
  allContentTypes :: [Http.MediaType]

instance (ContentType ct, AllContentTypes cts) => AllContentTypes (ct ': cts) where
  allContentTypes = Nel.toList (media @ct) <> allContentTypes @cts

instance AllContentTypes '[] where
  allContentTypes = mempty

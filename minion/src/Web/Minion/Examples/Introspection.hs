module Web.Minion.Examples.Introspection (app) where

import Data.CaseInsensitive qualified as CI
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.IO qualified
import GHC.Generics (Generic)
import Network.HTTP.Media
import Web.Minion
import Web.Minion.Auth.Basic
import Web.Minion.Introspect qualified as I
import Web.Minion.Media
import Web.Minion.Router

{-
GET api/post/:postId | Post api; Get post by ID
  Basic auth required
  Response: application/json

POST api/post/:postId | Post api; Create or update post
  Basic auth required
  Request body: text/plain
  Response: application/json

GET api/comments | Comments api; Get comments for post
  Query params: postId!, size?, page?
  Basic auth required
  Response: application/json

POST api/comments/:commentId | Comments api; Create or update comment
  Basic auth required
  Request body: text/plain
  Response: application/json

GET api/images/:pathToImage.. | Images api
  Basic auth required
  Response: raw bytes
-}
app :: IO ()
app = Data.Text.IO.putStrLn $ prettyApi api

prettyApi :: Router' Pretty Void m -> Text
prettyApi = Text.unlines . map prettyInfoToText . go
 where
  prependPath txt PrettyInfo{..} = PrettyInfo{path = txt : path, ..}
  addQueryParam qn isReq PrettyInfo{..} = PrettyInfo{queryParams = (Text.Encoding.decodeUtf8 qn, isReq) : queryParams, ..}
  addDescription txt PrettyInfo{..} = PrettyInfo{descriptions = txt : descriptions, ..}
  addHeader hn isReq PrettyInfo{..} = PrettyInfo{headers = (Text.Encoding.decodeUtf8 (CI.original hn), isReq) : headers, ..}
  addRequest req PrettyInfo{..} = PrettyInfo{request = req : request, ..}

  go :: Router' Pretty a m -> [PrettyInfo]
  go = \case
    Piece txt cont -> map (prependPath txt) (go cont)
    Capture _ txt cont -> map (prependPath (":" <> txt)) (go cont)
    Captures _ txt cont -> map (prependPath (":" <> txt <> "..")) (go cont)
    QueryParam @_ @presence qn _ cont -> map
      do addQueryParam qn (isRequired @presence)
      do go cont
    Description d cont -> map (addDescription (prettyDescription d)) (go cont)
    Middleware _ cont -> go cont
    Header @_ @presence hn _ cont -> map
      do addHeader hn (isRequired @presence)
      do go cont
    Request @r _ cont -> map (addRequest (prettyBody @r)) (go cont)
    Alt alts -> concatMap go alts
    Handle @o method _ -> [PrettyInfo [] [] [] [] (prettyBody @o) (Text.Encoding.decodeUtf8 method) []]
    MapArgs _ cont -> go cont
    HideIntrospection _ -> []

api :: Router' Pretty Void IO
api = "api" /> myAuth .> ["post" /> postApi, "comments" /> commentsApi, "images" /> imagesApi]
 where
  imagesApi = description "Images api" /> captures @String "pathToImage" .> handle @Chunks GET undefined
  postApi =
    description "Post api"
      /> capture @PostId "postId"
      .> [ description "Get post by ID" /> handleJson @Text GET undefined
         , description "Create or update post"
            /> reqPlainText @Text
            .> handleJson @() POST undefined
         ]
  commentsApi =
    description "Comments api"
      /> [
           [ queryParam' @PostId "postId"
              .> queryParam @Size "size"
              .> queryParam @Page "page"
              .> description "Get comments for post"
              /> handleJson @[Text] GET undefined
           , capture @CommentId "commentId"
              .> description "Create or update comment"
              /> reqPlainText @Text
              .> handleJson @() POST undefined
           ]
         ]
  myAuth =
    auth @'[Basic] @UserId
      (pure $ (undefined :: BasicAuthSettings IO UserId) :# HNil)
      undefined

data Pretty

type instance I.Introspection Pretty I.QueryParam = I.AbsolutelyNothing
type instance I.Introspection Pretty I.Capture = I.AbsolutelyNothing
type instance I.Introspection Pretty I.Captures = I.AbsolutelyNothing
type instance I.Introspection Pretty I.Header = I.AbsolutelyNothing
type instance I.Introspection Pretty I.Request = PrettyBody
type instance I.Introspection Pretty I.Response = PrettyBody
type instance I.Introspection Pretty I.Description = PrettyDescription

class PrettyBody a where
  prettyBody :: Text

class PrettyDescription a where
  prettyDescription :: a -> Text

instance (a ~ Text) => PrettyDescription a where
  prettyDescription = id

instance (AllContentTypes cts) => PrettyBody (ReqBody cts a) where
  prettyBody = "Request body: " <> Text.intercalate " or " (mediaTypes @cts)

instance PrettyBody Chunks where
  prettyBody = "Response: raw bytes"

instance PrettyBody (Auth '[Basic] a) where
  prettyBody = "Basic auth required"

instance (AllContentTypes cts) => PrettyBody (RespBody cts a) where
  prettyBody = "Response: " <> Text.intercalate " or " (mediaTypes @cts)

mediaTypes :: forall cts. (AllContentTypes cts) => [Text]
mediaTypes =
  nub
    . map
      do \a -> Text.Encoding.decodeUtf8 $ CI.original (mainType a) <> "/" <> CI.original (subType a)
    $ allContentTypes @cts

type PostId = Int
type UserId = Int
type CommentId = Int
type Size = Int
type Page = Int

data PrettyInfo = PrettyInfo
  { path :: [Text]
  , headers :: [(Text, Bool)]
  , queryParams :: [(Text, Bool)]
  , request :: [Text]
  , response :: Text
  , method :: Text
  , descriptions :: [Text]
  }
  deriving (Generic)

ifNotNull :: [x] -> ([x] -> a) -> Maybe a
ifNotNull [] _ = Nothing
ifNotNull list f = Just $ f list

reqOpt :: (Text, Bool) -> Text
reqOpt (a, r) = a <> if r then "!" else "?"

prettyInfoToText :: PrettyInfo -> Text
prettyInfoToText PrettyInfo{..} =
  Text.unlines $
    method <> " " <> Text.intercalate "/" path <> maybe "" (" | " <>) (ifNotNull descriptions (Text.intercalate "; "))
      : map
        ("  " <>)
        ( catMaybes
            [ ("Query params: " <>) <$> ifNotNull queryParams (Text.intercalate ", " . map reqOpt)
            , ("Headers: " <>) <$> ifNotNull headers (Text.intercalate ", " . map reqOpt)
            ]
            <> request
            <> [response]
        )

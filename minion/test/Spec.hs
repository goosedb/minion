{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# HLINT ignore "Use isNothing" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Concurrent qualified as Conc
import Control.Lens hiding ((.=), (.>))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as J
import Data.Aeson.Lens (key)
import Data.Bifunctor (Bifunctor (..))
import Data.Bool (bool)
import Data.ByteString.Builder qualified as Binary
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.CaseInsensitive qualified as CI
import Data.Functor (($>), (<&>))
import Data.IORef qualified as IORef
import Data.IORef qualified as Ref
import Data.List.NonEmpty qualified as Nel
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Network.HTTP.Media qualified as Http
import Network.HTTP.Types qualified as Http
import Network.Wai (responseStatus)
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as Wai.Internal
import Test.Hspec (
  Expectation,
  HasCallStack,
  Spec,
  SpecWith,
  describe,
  hspec,
  it,
  shouldSatisfy,
 )
import Text.Read (readEither, readMaybe)
import Web.FormUrlEncoded
import Web.Minion
import Web.Minion qualified as Minion
import Web.Minion.Json
import Web.Minion.Media (ContentType (..))
import Web.Minion.Media.FormUrlEncoded
import Web.Minion.Media.PlainText (PlainText)
import Web.Minion.Request.Body (Decode (..))

{-# INLINE compile #-}
compile :: Minion.Router Void IO -> IO (Wai.Request -> IO Wai.Response)
compile r = do
  response <- Conc.newEmptyMVar
  let compiled get = do
        Minion.serve r get \resp -> do
          Conc.putMVar response resp
          pure Wai.Internal.ResponseReceived
  pure (compiled >=> const (Conc.readMVar response))

compileMD :: Minion.Router Void (ReaderT Minion.MatchedData IO) -> IO (Wai.Request -> IO Wai.Response)
compileMD r = do
  response <- Conc.newEmptyMVar
  let compiled get = do
        Minion.serveWithSettings Minion.defaultMinionSettings{Minion.withMatchedData = \d -> local (const d)} r get \resp -> do
          Conc.putMVar response resp
          pure Wai.Internal.ResponseReceived
  pure \req -> runReaderT (compiled req) (Minion.MatchedData{path = [], headers = [], query = [], method = GET}) >>= const (Conc.readMVar response)

root :: Minion.Router Void m -> Minion.Router Void m
root = id

{-# INLINE sendTo #-}
sendTo :: Wai.Request -> Minion.Router Void IO -> IO Wai.Response
sendTo req server = compile server >>= ($ req)

sendToMD :: Wai.Request -> Minion.Router Void (ReaderT Minion.MatchedData IO) -> IO Wai.Response
sendToMD req server = compileMD server >>= ($ req)

sendToIO :: IO Wai.Request -> Minion.Router Void IO -> IO Wai.Response
sendToIO req server = compile server >>= \s -> req >>= \r -> s r

test :: (HasCallStack) => String -> Expectation -> SpecWith ()
test = it @Expectation

data ShowResponse a = ShowResponse {status :: Http.Status, headers :: [Http.Header], body :: a}
  deriving (Show)

class ToShowResponse a where
  toShowResponse :: Wai.Response -> IO (ShowResponse a)

instance ToShowResponse J.Value where
  toShowResponse r = do
    ref <- Ref.newIORef mempty
    (\(_, _, a) -> a (\sb -> sb (\b -> Ref.modifyIORef ref (<> b)) (pure ()))) (Wai.responseToStream r)
    body <- Ref.readIORef ref >>= either (\e -> fail $ "failed to parse json: " <> e) pure . J.eitherDecode . Binary.toLazyByteString
    pure $ ShowResponse (Wai.responseStatus r) (Wai.responseHeaders r) body

instance ToShowResponse Text where
  toShowResponse r = do
    ref <- Ref.newIORef mempty
    (\(_, _, a) -> a (\sb -> sb (\b -> Ref.modifyIORef ref (<> b)) (pure ()))) (Wai.responseToStream r)
    body <- Ref.readIORef ref <&> Text.decodeUtf8 . Bytes.Lazy.toStrict . Binary.toLazyByteString
    pure $ ShowResponse (Wai.responseStatus r) (Wai.responseHeaders r) body

data NoParse = NoParse
  deriving (Show)

instance ToShowResponse NoParse where
  toShowResponse r = do
    pure $ ShowResponse (Wai.responseStatus r) (Wai.responseHeaders r) NoParse

responseShouldSatisfy :: (HasCallStack, ToShowResponse a, Show a) => IO Wai.Response -> (ShowResponse a -> Bool) -> Expectation
responseShouldSatisfy resp validate = resp >>= (toShowResponse >=> (`shouldSatisfy` validate))

responseShouldFailWith :: (HasCallStack) => IO Wai.Response -> (ShowResponse NoParse -> Bool) -> Expectation
responseShouldFailWith resp validate = resp >>= (toShowResponse @NoParse >=> (`shouldSatisfy` validate))

responseShouldFailWithBody :: (HasCallStack) => IO Wai.Response -> (ShowResponse Text -> Bool) -> Expectation
responseShouldFailWithBody resp validate = resp >>= (toShowResponse @Text >=> (`shouldSatisfy` validate))

withQueryParams :: [(Text, Text)] -> Wai.Request -> Wai.Request
withQueryParams qps get =
  let newQueryString = Wai.queryString get <> map (bimap Text.encodeUtf8 (Just . Text.encodeUtf8)) qps
   in get{Wai.queryString = newQueryString}

withPath :: [Text] -> Wai.Request -> Wai.Request
withPath path get = get{Wai.pathInfo = path}

withHeader :: Http.Header -> Wai.Internal.Request -> Wai.Internal.Request
withHeader header get = get{Wai.requestHeaders = header : Wai.requestHeaders get}

withJsonBody :: (J.ToJSON a) => a -> Wai.Request -> IO Wai.Request
withJsonBody v req = feed >>= \f -> pure $ Wai.setRequestBodyChunks f req
 where
  feed = do
    alreadyFed <- IORef.newIORef False
    pure do
      IORef.readIORef alreadyFed >>= bool (IORef.writeIORef alreadyFed True $> Bytes.Lazy.toStrict (J.encode v)) (pure mempty)

withTextBody :: Text -> Wai.Request -> IO Wai.Request
withTextBody v req = feed >>= \f -> pure $ Wai.setRequestBodyChunks f req
 where
  feed = do
    alreadyFed <- IORef.newIORef False
    pure do
      IORef.readIORef alreadyFed >>= bool (IORef.writeIORef alreadyFed True $> Text.encodeUtf8 v) (pure mempty)

get :: Wai.Request
get = Wai.defaultRequest

post :: Wai.Request
post = Wai.defaultRequest{Wai.requestMethod = POST}

put :: Wai.Request
put = Wai.defaultRequest{Wai.requestMethod = PUT}

withContentType :: forall ct. (ContentType ct) => Wai.Request -> Wai.Request
withContentType req = req{Wai.requestHeaders = Wai.requestHeaders req <> [("Content-Type", Http.renderHeader $ Nel.head $ media @ct)]}

main :: IO ()
main = hspec do
  describe "Router" do
    -- describe "Response" responseSpec
    describe "Request" do
      describe "Body" bodySpec
      describe "Method" methodSpec
      describe "Path" pathSpec
      describe "Capture" captureSpec
      describe "Captures" capturesSpec
      describe "Query" do
        describe "Query param" queryParamSpec
        describe "Query params" queryParamsSpec
        describe "Query form" queryFormSpec
        describe "Query flag" queryFlagSpec

(.->) :: a -> b -> (a, b)
(.->) = (,)

txt :: Text -> Text
txt = id

int :: Int -> Int
int = id

list :: forall a. [a] -> [a]
list = id

data Foo = Foo {foo :: Int, bar :: Bool} deriving (Generic, J.ToJSON, J.FromJSON)
data Baz = Baz {baz :: [Int], qux :: String} deriving (Generic, J.ToJSON, J.FromJSON)

instance Decode PlainText Foo where
  decode =
    first Text.pack
      . ( \case
            [foo, bar] -> Foo <$> readEither foo <*> readEither bar
            strange -> Left (unwords strange)
        )
      . map Text.unpack
      . Text.words
      . Text.decodeUtf8
      . Bytes.Lazy.toStrict

bodySpec :: Spec
bodySpec = do
  test "parse body depending on method" do
    let server =
          root
            /> "api"
            /> "foo"
            /> "bar"
            /> [ reqJson @Foo .> handleJson @J.Value GET \Foo{..} -> pure $ J.object ["foo" .= J.object ["foo" .= foo, "bar" .= bar]]
               , reqJson @Baz .> handleJson @J.Value POST \Baz{..} -> pure $ J.object ["baz" .= J.object ["baz" .= baz, "qux" .= qux]]
               ]

    (get & withPath ["api", "foo", "bar"] & withJsonBody Foo{foo = 1, bar = False})
      `sendToIO` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["foo" .= J.object ["foo" .= int 1, "bar" .= False]]

    (post & withPath ["api", "foo", "bar"] & withJsonBody Baz{baz = [1, 2], qux = "hello"})
      `sendToIO` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["baz" .= J.object ["baz" .= list [int 1, 2], "qux" .= txt "hello"]]

  test "parse body depending on content-type" do
    let server =
          root
            /> "api"
            /> "foo"
            /> "bar"
            /> reqBody @'[Json, PlainText] @Foo
            .> handleJson POST \Foo{..} -> pure $ J.object ["foo" .= J.object ["foo" .= foo, "bar" .= bar]]

    (post & withPath ["api", "foo", "bar"] & withContentType @Json & withJsonBody Foo{foo = 1, bar = False})
      `sendToIO` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["foo" .= J.object ["foo" .= int 1, "bar" .= False]]

    (post & withPath ["api", "foo", "bar"] & withContentType @PlainText & withTextBody "10 True")
      `sendToIO` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["foo" .= J.object ["foo" .= int 10, "bar" .= True]]

    (post & withPath ["api", "foo", "bar"] & withContentType @FormUrlEncoded & withTextBody "dont care")
      `sendToIO` server
      `responseShouldFailWith` \ShowResponse{..} -> status == Http.status415

  test "parse first content-type if not specified" do
    let api cont = root /> "api" /> "foo" /> "bar" /> cont
    let handle Foo{..} = pure $ J.object ["foo" .= J.object ["foo" .= foo, "bar" .= bar]]
    let serverJson = api /> reqBody @'[Json, PlainText] @Foo .> handleJson POST handle
    let serverText = api /> reqBody @'[PlainText, Json] @Foo .> handleJson POST handle

    (post & withPath ["api", "foo", "bar"] & withJsonBody Foo{foo = 1, bar = False})
      `sendToIO` serverJson
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["foo" .= J.object ["foo" .= int 1, "bar" .= False]]

    (post & withPath ["api", "foo", "bar"] & withTextBody "10 True")
      `sendToIO` serverText
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["foo" .= J.object ["foo" .= int 10, "bar" .= True]]

methodSpec :: Spec
methodSpec = do
  let server =
        root
          /> "api"
          /> [ "foo" /> "bar" /> handlePlainText GET (pure $ txt "/foo/bar get")
             , "foo" /> "bar" /> handlePlainText POST (pure $ txt "/foo/bar post")
             , "baz"
                 /> "qux"
                 /> [ handlePlainText GET (pure $ txt "/baz/qux get")
                    , handlePlainText POST (pure $ txt "/baz/qux post")
                    ]
             ]
  test "find route with specified method" do
    withPath ["api", "foo", "bar"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == txt "/foo/bar get"

    withPath ["api", "foo", "bar"] post
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == txt "/foo/bar post"

    withPath ["api", "baz", "qux"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == txt "/baz/qux get"

    withPath ["api", "baz", "qux"] post
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == txt "/baz/qux post"

  test "fail to find route with specified method" do
    withPath ["api", "foo", "bar"] put
      `sendTo` server
      `responseShouldFailWith` \ShowResponse{..} -> status == Http.status404

    withPath ["api", "baz", "qux"] put
      `sendTo` server
      `responseShouldFailWith` \ShowResponse{..} -> status == Http.status404

captureSpec :: Spec
captureSpec = do
  test "static pieces have higher priority" do
    let server =
          root
            /> "api"
            /> [ capture @String "name" .> handlePlainText GET (\_ -> pure $ txt "dynamic")
               , "John" /> handlePlainText GET (pure $ txt "static")
               ]
    withPath ["api", "Mary"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == txt "dynamic"

    withPath ["api", "John"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == txt "static"

  test "fallback to next if parsing fail" do
    let server =
          root
            /> "api"
            /> [ capture @Int "id" .> "foo" /> handleJson GET (\_id -> pure $ J.object ["id" .= _id])
               , capture @Bool "enabled" .> handleJson GET (\enabled -> pure $ J.object ["enabled" .= enabled])
               , capture @String "name" .> handleJson GET (\name -> pure $ J.object ["name" .= name])
               ]

    withPath ["api", "6", "foo"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["id" .= int 6]

    withPath ["api", "true"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["enabled" .= True]

    withPath ["api", "John"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["name" .= txt "John"]

  test "throw last parsing error" do
    let server =
          root
            /> "api"
            /> [ capture @Int "id" .> "foo" /> handleJson GET (\_id -> pure $ J.object ["id" .= _id])
               , capture @Bool "enabled" .> handleJson GET (\enabled -> pure $ J.object ["enabled" .= enabled])
               ]

    withPath ["api", "6", "foo"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["id" .= int 6]

    withPath ["api", "true"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["enabled" .= True]

    withPath ["api", "John"] get
      `sendTo` server
      `responseShouldFailWithBody` \ShowResponse{..} -> and @[] [status == Http.status400, body == "could not parse: `john'"]

capturesSpec :: Spec
capturesSpec = do
  test "fallback to next if parsing fail" do
    let server =
          root
            /> "api"
            /> [ captures @Int "id" .> handleJson GET (\ids -> pure $ J.object ["ids" .= ids])
               , captures @Bool "flags" .> handleJson GET (\flags -> pure $ J.object ["flags" .= flags])
               , captures @String "items" .> handleJson GET (\items -> pure $ J.object ["items" .= items])
               ]

    withPath ["api", "6", "10", "500"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["ids" .= list [int 6, 10, 500]]

    withPath ["api", "true", "false", "true"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["flags" .= list [True, False, True]]

    withPath ["api", "foo", "bar", "baz"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["items" .= list [txt "foo", "bar", "baz"]]

  test "throw last parsing error" do
    let server =
          root
            /> "api"
            /> [ captures @Int "id" .> handleJson GET (\ids -> pure $ J.object ["ids" .= ids])
               , captures @Bool "flags" .> handleJson GET (\flags -> pure $ J.object ["flags" .= flags])
               ]

    withPath ["api", "6", "10", "500"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["ids" .= list [int 6, 10, 500]]

    withPath ["api", "true", "false", "true"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["flags" .= list [True, False, True]]

    withPath ["api", "foo", "bar", "baz"] get
      `sendTo` server
      `responseShouldFailWithBody` \ShowResponse{..} -> and @[] [status == Http.status400, body == "could not parse: `foo'"]

pathSpec :: Spec
pathSpec = do
  let pt = handlePlainText GET . pure . txt . fromString . show @Int
  let server =
        root
          [ "api"
              /> [ "aaa"
                     /> "bbb"
                     /> [ "ccc" /> "ddd" /> pt 1
                        , "eee" /> pt 2
                        ]
                 , "fff" /> "ggg" /> pt 3
                 ]
          , "internal" /> "zzz" /> pt 4
          ]

  test "find route by path" do
    withPath ["api", "aaa", "bbb", "ccc", "ddd"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == txt "1"

    withPath ["api", "aaa", "bbb", "eee"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == txt "2"

    withPath ["api", "fff", "ggg"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == txt "3"

    withPath ["internal", "zzz"] get
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == txt "4"

  test "fail to find route by path" do
    withPath ["api", "aaa", "bbb", "ccc", "ddd", "eee"] get
      `sendTo` server
      `responseShouldFailWith` \ShowResponse{..} -> status == Http.status404

    withPath ["api", "aaa", "bbb", "ccc"] get
      `sendTo` server
      `responseShouldFailWith` \ShowResponse{..} -> status == Http.status404

    withPath ["api", "aaa", "bbb"] get
      `sendTo` server
      `responseShouldFailWith` \ShowResponse{..} -> status == Http.status404

    withPath ["api", "fff"] get
      `sendTo` server
      `responseShouldFailWith` \ShowResponse{..} -> status == Http.status404

    withPath [] get
      `sendTo` server
      `responseShouldFailWith` \ShowResponse{..} -> status == Http.status404

queryParamSpec :: Spec
queryParamSpec = do
  describe "required" do
    let server = root do
          "api" /> "query" /> queryParam @Required @String "foo" .> queryParam @Required @String "bar" .> handleJson @J.Value GET method
        method foo bar = pure $ J.object ["foo" .= foo, "bar" .= bar]
        baseRequest = get & withPath ["api", "query"]

    test "collect query" do
      let request = baseRequest & withQueryParams ["foo" .-> "hi", "bar" .-> "bye"]
      request
        `sendTo` server
        `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["foo" .= txt "hi", "bar" .= txt "bye"]

    test "fail to collect query" do
      let request = baseRequest & withQueryParams ["foo" .-> "hi"]
      request
        `sendTo` server
        `responseShouldFailWith` \ShowResponse{..} -> Http.status400 == status

  describe "optional" do
    test "collect query" do
      let server = root do
            "api" /> "query" /> queryParam @Optional @String "qux" .> queryParam @Optional @String "baz" .> handleJson @J.Value GET method
          method qux baz = pure $ J.object ["qux" .= qux, "baz" .= baz]

      let request = get & withPath ["api", "query"] & withQueryParams ["qux" .-> "hi"]
      request
        `sendTo` server
        `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["qux" .= txt "hi", "baz" .= J.Null]

  describe "mixed" do
    let server = root do
          "api" /> "query" /> queryParam @Optional @String "qux" .> queryParam @Required @String "baz" .> handleJson @J.Value GET method
        method qux baz = pure $ J.object ["qux" .= qux, "baz" .= baz]
        baseRequest = get & withPath ["api", "query"]

    test "collect query" do
      let request = baseRequest & withQueryParams ["baz" .-> "hi"]
      request
        `sendTo` server
        `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["baz" .= txt "hi", "qux" .= J.Null]

    test "fail to collect query" do
      let request = baseRequest & withQueryParams []
      request
        `sendTo` server
        `responseShouldFailWith` \ShowResponse{..} -> status == Http.status400

queryParamsSpec :: Spec
queryParamsSpec = do
  describe "required" do
    let server = root do
          "api" /> "query" /> queryParams @Required @Int "foo" .> handleJson @J.Value GET method
        method foo = pure $ J.object ["foo" .= foo]
        baseRequest = get & withPath ["api", "query"]

    test "positive" do
      let request = baseRequest & withQueryParams ["foo" .-> "1", "foo" .-> "2", "foo" .-> "3"]
      request
        `sendTo` server
        `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["foo" .= list [int 1, 2, 3]]

    test "negative" do
      let request = baseRequest
      request
        `sendTo` server
        `responseShouldFailWith` \ShowResponse{..} -> status == Http.status400

  test "optional" do
    let server = root do
          "api" /> "query" /> queryParams @Optional @Int "foo" .> queryParams @Optional @Int "bar" .> handleJson @J.Value GET method
        method foo bar = pure $ J.object ["foo" .= foo, "bar" .= bar]
        request = get & withPath ["api", "query"] & withQueryParams ["foo" .-> "1", "foo" .-> "2", "foo" .-> "3"]

    request
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["foo" .= list [int 1, 2, 3], "bar" .= J.Null]

queryFlagSpec :: Spec
queryFlagSpec = do
  describe "required" do
    let server = root do
          "api"
            /> "query"
            /> queryFlag @Required "foo"
            .> queryFlag @Required "bar"
            .> queryFlag @Required "baz"
            .> queryFlag @Required "qux"
            .> queryFlag @Required "yoi"
            .> handleJson @J.Value GET method
        method (QueryFlag foo) (QueryFlag bar) (QueryFlag baz) (QueryFlag qux) (QueryFlag yoi) =
          pure $
            J.object
              [ "foo" .= foo
              , "bar" .= bar
              , "baz" .= baz
              , "qux" .= qux
              , "yoi" .= yoi
              ]
        baseRequest = get & withPath ["api", "query"]

    test "positive" do
      let request = baseRequest & withQueryParams ["foo" .-> "1", "bar" .-> "", "baz" .-> "true", "qux" .-> "false", "yoi" .-> "something_else"]
      request
        `sendTo` server
        `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["foo" .= True, "bar" .= True, "baz" .= True, "qux" .= False, "yoi" .= False]

    test "negative" do
      let request = baseRequest & withQueryParams ["foo" .-> "1", "bar" .-> "", "baz" .-> "true", "qux" .-> "false"]
      request
        `sendTo` server
        `responseShouldFailWith` \ShowResponse{..} -> status == Http.status400
  test "optional" do
    let getQueryFlag (QueryFlag a) = a
        server = root do
          "api"
            /> "query"
            /> queryFlag @Optional "foo"
            .> queryFlag @Optional "bar"
            .> queryFlag @Optional "baz"
            .> handleJson @J.Value GET method
        method (fmap getQueryFlag -> foo) (fmap getQueryFlag -> bar) (fmap getQueryFlag -> baz) =
          pure $
            J.object
              [ "foo" .= foo
              , "bar" .= bar
              , "baz" .= baz
              ]
        request = get & withPath ["api", "query"] & withQueryParams ["foo" .-> "false", "baz" .-> ""]

    request
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.object ["foo" .= False, "bar" .= J.Null, "baz" .= True]

data SomeQueryForm = SomeQueryForm {foo :: Int, bar :: [String], baz :: Maybe String}
  deriving (Generic, Show)
  deriving anyclass (FromForm, ToForm, J.ToJSON)

queryFormSpec :: Spec
queryFormSpec = do
  test "positive" do
    let server = root do
          "api" /> "query" /> queryParamsForm @SomeQueryForm .> handleJson @J.Value GET method
        method form = pure $ J.toJSON form
        baseRequest = get & withPath ["api", "query"]

    let request = baseRequest & withQueryParams ["foo" .-> "5", "bar" .-> "aaa", "bar" .-> "bbb", "baz" .-> "zzz"]
    request
      `sendTo` server
      `responseShouldSatisfy` \ShowResponse{..} -> body == J.toJSON SomeQueryForm{foo = 5, bar = ["aaa", "bbb"], baz = Just "zzz"}

  test "negative" do
    let server = root do
          "api" /> "query" /> queryParamsForm @SomeQueryForm .> handleJson @J.Value GET method
        method form = pure $ J.toJSON form
        baseRequest = get & withPath ["api", "query"]

    let request = baseRequest & withQueryParams ["bar" .-> "aaa", "bar" .-> "bbb", "baz" .-> "zzz"]
    request
      `sendTo` server
      `responseShouldFailWith` \ShowResponse{..} -> status == Http.status400

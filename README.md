# Minion

Minion is Haskell library for developing web applications. It stands between [Scotty](https://hackage.haskell.org/package/scotty) and [Servant](https://hackage.haskell.org/package/servant-server)  

|                  | Scotty | Minion                    | Servant |
| ---------------- | ------ | ------------------------- | ------- |
| As simple as ABC | Yes    | No                        | No      |
| At term level    | Yes    | Yes                       | No      |
| Typesafe         | No     | Yes                       | Yes     |
| Introspectable   | No     | Yes                       | Yes     |
| Generated client | No     | Yes (via TemplateHaskell) | Yes     |

  
Since Minion defines servers at the term level, it's easier to start and without excess verbosity.

# Guide
  1. [Getting started](#getting-started)
  2. [Router](#router)
  3. [Combinators](#combinators)
      1. [Path](#path)
      2. [Query params](#query-params)
      3. [Headers](#headers)
      4. [Request](#request)
      5. [Handler](#handler)
  4. [Auth](#auth)
  5. [Introspection](#introspection)

## Getting started

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where
import Web.Minion
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = Warp.run 9001 app -- (1)

app :: ApplicationM IO
app = serve api -- (2)

api :: Router Void IO
api = -- (3)
  "api"
    /> [ "about" 
          /> handleBody @Ok @'[PlainText] @String GET about
       , "hello" 
          /> capture @String "name" 
          .> handleBody @Ok @'[PlainText] @String GET hello
       ]
 where
  -- (4)
  about = pure "Hello-World Minion server"
  hello name = pure $ "Hello, " <> name <> "!"
```
### Explanation
1. The Minion server is launched in the same way as any other WAI-based server.
2. Minion provides a serve function that renders the minion-router into a WAI application.
3. The API is described using basic combinators which will be discussed in the next section.
4. Handlers for API endpoints are directly defined within the API description.

## Router
The main type of `Minion` is `Router`, which has several type parameters:
```haskell
data Router' (i :: [Type]) (ts :: Type) m

-- Assuming that most of the time you won't need any introspection, Minion provides a type alias
type Router = Router' '[]
```
1. A list of introspections, such as `OpenApi3` or `Client`.
2. Arguments required by the router to start up.
3. Monad in which your server logic will run.

It's worth paying attention specifically to the second point because it might not be immediately obvious. This is due to the fact that handler typing in Minion works not from the root but from the handlers themselves.
For example, if you replace `hello` with a hole:
```haskell
  "api"
    /> [ "about" 
          /> handleBody @Ok @'[PlainText] @String GET about
       , "hello" 
          /> capture @String "name" 
          .> handleBody @Ok @'[PlainText] @String GET _
       ]
```
You'll find that its type is `String -> IO String`:
```haskell
    • Found hole: _ :: [Char] -> IO String
```
This means that in order for `hello` to work properly, it somehow needs to know where to obtain the `String` from. Therefore, we wrap `handleBody @Ok @'[PlainText] @String GET hello` inside `capture @String "name"`.  
Therefore, our ultimate goal is to create a `Router' i Void m`, meaning this is a router that can be executed.

## Combinators
Minion provides a set of basic combinators that you can use to create new ones. A combinator is essentially a function of type `Router -> Router`.  
There are two types of combinators: those that capture values and those that don't.

```haskell
type ValueCombinator i v ts m = Router' i (ts :+ v) m -> Router' i ts m

type Combinator i ts m = Router' i ts m -> Router' i ts m
```
`ValueCombinator` is a function that describes how to extract a value of type `v` from a request. In some sense, `Router' i (ts :+ v) m -> Router' i ts m` is analogous to `(v -> Router' i ts m) -> Router' i ts m`, but the actual passing of `v` is deferred until the handler stage. If this nuance seems too complicated right now, feel free to skip it :)

### Path 
These combinators are defined in the module `Web.Minion.Request.Path` and available from `Web.Minion`.

* `piece` (or string literal with `OverloadedStrings` enabled): matches a static path segment.
* `capture`: extracts and parses a value from a single path segment.
* `captures`: captures and parses all remaining path segments.

For example
```haskell
api :: Router Void IO
api =
  "api" -- (1)
    /> piece "internal" -- (2)
    /> capture @String "name" -- (3)
    .> captures @String "stuff" -- (4)
    .> handleBody @Ok @'[PlainText] @String GET _ -- (5)

-- ghc 
    • Found hole: _ :: [Char] -> [[Char]] -> IO String
```
1. Implicit `piece` via `OverloadedStrings`
2. Although stylistically it's not very good to mix implicit `piece` through `OverloadedStrings` and explicit one through a function, here it's done for demonstration purposes.
3. Capturing a single path segment
4. Capturing the remainder of the path
5. Handler function, compiler infers its type as `String -> [String] -> IO String`. Thus, the path `/api/internal/john/foo/bar/baz` would invoke the handler with arguments `"john"` and `["foo", "bar", "baz"]`.

Note that `/>` is used after combinators that do not capture new values, while `.>` follows combinators that perform capturing. Internally, each of them is simply `$`, but with more concrete types. For a fork, you can use the explicit combinator `alt`, or simply use a list with `OverloadedLists`. We also have a combinator `!>` that captures a value, checks its validity, but doesn't pass it to the handler.
### Query params

In Minion, you can extract query parameters from requests in various ways, which are defined in the following modules:
* `Web.Minion.Request.Query.Flag`
* `Web.Minion.Request.Query.Form`
* `Web.Minion.Request.Query.Param`
* `Web.Minion.Request.Query.Params`
For example:
```haskell
type Comment = String

api :: Router Void IO
api =
  "api"
    /> "comments"
    /> [ "v1"
           /> queryParam @Required @Int "page" -- (1)
           .> queryParam @Optional @Int "limit" -- (2)
           .> queryFlag @Optional "approved" -- (3)
           .> queryParams @Optional @String "tag" -- (4)
           .> handleBody @Ok @'[Json] @[Comment] GET _ -- (5)
       , "v2"
           /> queryParamsForm @CommentsQuery -- (6)
           .> handleBody @Ok @'[Json] @[Comment] GET _ -- (7)
       ]


-- ghc
    • Found hole:
        _ :: Int
          -> Maybe Int
          -> Maybe Bool
          -> Maybe (NonEmpty String)
          -> IO [Comment]
    • Found hole: _ :: CommentsQuery -> IO [Comment]
```
1. Expects a query parameter named `page` of type `Int`.
2. Expects an optional query parameter named `limit` of type `Int`.
3. Expects an optional boolean query flag named `approved`.
4. Captures all query parameters named `tag` into a list.
5. Handler function, compiler infers its type as `Int -> Maybe Int -> Maybe Bool -> Maybe (NonEmpty Int) -> IO [Comment]`.
6. Captures multiple query parameters at once using the `FromForm` parser, which is derived via `Generic`. Note that `queryFlag` is different from `queryParam Bool`, since the former parses `?approved`, `?approved=1`, `?approved=true` as `True`, whereas the latter only accepts `?approved=true`.

There is also lenient version for `queryParam`: `queryParamLenient`, which in case of parsing error passes the error to the handler instead of throwing a `BadRequest`.

### Headers
You can extract headers from requests using the following combinators defined in the module `Web.Minion.Request.Header` and available via `Web.Minion`:

* `header`
* `headerLenient`

For example:

```haskell
api :: Router Void IO
api =
  "api"
    /> [ "strict" /> xCustomHeader .> handleBody @Ok @'[PlainText] @String GET _
       , "lenient" /> xAnotherCustomHeader .> handleBody @Ok @'[PlainText] @String GET _
       ]

where
  xCustomHeader = 
    header @Required @Int "X-Custom-Header" \_ -> 
      pure . Bytes.length . NonEmpty.head

  xAnotherCustomHeader = 
    headerLenient @Optional @Int @String "X-Custom-Header" \_ -> 
      pure . (\i -> if even i then Right i else Left "not even") . Bytes.length . NonEmpty.head

-- ghc
    • Found hole: _ :: Int -> IO String
    • Found hole:
        _ :: Maybe (Either Data.Text.Internal.Text Int) -> IO String
```

Since HTTP allows sending multiple headers with the same name, the `header` combinator requires a closure capable of processing a non-empty list of values associated with the given header name:

```haskell
header ::
    forall a m i ts.
    (I.Introspection i I.Header a, MonadThrow m) =>
    Http.HeaderName ->
    (MakeError -> NonEmpty Bytes.ByteString -> m a) -> -- NonEmpty
    ValueCombinator i (WithHeader presence Strict m a) ts m
```

Of course, in most cases it will be sufficient to use either `NonEmpty.head` or `NonEmpty.last`.

The `headerLenient` combinator works similarly to `queryParamLenient`, allowing an error in header processing to propagate into the handler:

```haskell
headerLenient :: forall a e m ts i.
    (I.Introspection i I.Header a, MonadThrow m) =>
    Http.HeaderName ->
    (MakeError -> NonEmpty Bytes.ByteString -> m (Either e a)) -> -- Allows passing error on type `e` to the handler
    ValueCombinator i (WithHeader presence (Lenient e) m a) ts m
```

Note that when handling headers, you have access to all capabilities provided by your router's underlying monad `m`.

### Request
In order to extract the request body, you can utilize the functions `reqBody` and `reqBodyStream`, both of which are defined in the module `Web.Minion.Request.Body` and accessible through `Web.Minion`. Here's an example:

```haskell
data FooRequest = FooRequest
  { foo :: Int
  , bar :: Int
  }
  deriving (Generic, FromJSON)

api :: Router Void IO
api = "api"  
  /> "foo"
  /> reqBody @'[Json, PlainText] @FooRequest 
  .> handleBody @Ok @'[PlainText] @String POST _

-- ghc
    • Found hole: _ :: FooRequest -> IO String
```

The `reqBody` combinator accepts a list of content types from which the request body can be parsed, along with the request type itself (`@FooRequest`). Since no way has been provided to parse `FooRequest` from plain text, the compiler fails compilation with the following error:

```haskell
• No instance for ‘Web.Minion.Codec.Decode.Decode PlainText FooRequest’
    arising from a use of ‘reqBody’
```

This instance could be implemented as follows:

```haskell
instance Decode PlainText FooRequest where 
  decode (traverse (readMaybe . Text.Lazy.unpack) . Text.Lazy.words . Text.Lazy.decodeUtf8 -> Just [foo, bar]) = pure FooRequest {..}
  decode _ = Left "Failed to parse FooRequest"
```

After implementing this instance, the `/api/foo` endpoint will accept:

* Requests with `Content-Type: text/plain` and a body like `"5, 6"`
* Requests with `Content-Type: application/json` and a body like `{"foo": 5, "bar": 6}`

If the request body needs to be streamed, consider using `reqBodyStream`, which requires providing an instance of `DecodeStream` for every listed content type.

For more complex scenarios (such as multipart, implemented in the `minion-wai-extra` package, or websockets, implemented in the `minion-websockets` package), it's recommended to use the basic combinator `Request`.:

```haskell
  Request ::
    forall r m i ts.
    (I.Introspection i I.Request r, IsRequest r) =>
    -- | .
    (ErrorBuilder -> Wai.Request -> m r) ->
    Router' i (ts :+ WithReq m r) m ->
    Router' i ts m
```

The combinator allows extracting arbitrary data from the Wai.Request while utilizing all features offered by the monad `m`. Note that reading the request body from `Wai.Request` is a destructive operation—once consumed, it can't be accessed again. Therefore, always strive to read the request body using the `reqBody` combinator (or another that reads the request body) immediately before the handler.

### Handler

Finally, after capturing all necessary components, it's time to process the request. For this purpose, there is a function called `handle`, which converts anything satisfying the typeclasses `ToResponse` and `CanRespond` into an HTTP response. In previous examples, we used a more specific function named `handleBody`, which operates on the `RespBody` type:

```haskell
newtype RespBody status cts a = RespBody a

handleBody ::
  forall status cts o m ts i st.
  (HandleArgs ts st m) =>
  (IsResponse m (RespBody status cts o)) =>
  (I.Introspection i I.Response (RespBody status cts o)) =>
  Http.Method ->
  (DelayedArgs st ~> m o) ->
  Router' i ts m
```
The `handleBody` function expects:

* An HTTP status code for the response;
* A list of possible Content-Type formats into which the response may be converted;
* The HTTP method being handled;
* Finally, a handler function.

If streaming the response body is intended, one should instead use the `handleBodyStream` function, which would require instances of `EncodeStream` for each Content-Type specified.

### Auth
Minion provides a combinator for authentication:

```haskell
auth ::
  forall auths a m ctx ts i.
  (I.Introspection i I.Request (Auth auths a)) =>
  (UnwindAuth ctx auths m a) =>
  (MonadThrow m) =>
  m (HList ctx) -> -- (1)
  (MakeError -> AuthResult Void -> m Void) -> -- (2)
  ValueCombinator i (WithReq m (Auth auths a)) ts m
```

1. Context containing settings for each authentication method.
2. Function for handling failed authentication.

To use an authentication method, it must provide an instance of the `IsAuth` type class:

```haskell
class IsAuth (auth :: Type) m a where
  type Settings auth m a :: Type
  toAuth :: Settings auth m a -> ErrorBuilder -> Wai.Request -> m (AuthResult a)
```
An implementation for Basic auth can be seen in the `Web.Minion.Auth.Basic` module. It’s simple enough to understand yet fully functional.

Example API with authorization:
```haskell
type Env = [BasicAuth]
type M = ReaderT Env IO

app :: IO (ApplicationM IO)
app = do
  -- (1)
  let users = [ BasicAuth "alice" "123", BasicAuth "bob" "312", BasicAuth "admin" "admin" ]
  pure $ \req resp -> runReaderT (serve api req resp) users

api :: Router Void M
api = "api" /> "auth" /> "basic" /> myAuth .> handle @(NoBody Ok) GET endpoint
 where
  endpoint (UserId userId) = liftIO do
    putStrLn ("Called by " <> show userId) $> NoBody

newtype UserId = UserId Int

-- (2)
basicAuthSettings :: BasicAuthSettings M UserId
basicAuthSettings = 
  BasicAuthSettings \_ ba -> maybe BadAuth (Authenticated . UserId) . elemIndex ba <$> ask
    
-- (3)
myAuth :: ValueCombinator '[] (WithReq M (Auth '[Basic] UserId)) ts M
myAuth = auth @'[Basic] @UserId (pure $ basicAuthSettings :# HNil) \makeError -> \case
  _ -> do
    liftIO $ putStrLn "Unauthorized!"
    throwM $ makeError (statusOf unauthorized) mempty
```
1. List of “known” users. Hardcoded purely for demonstration purposes.
2. Logic for processing basic authentication. In our case, we simply search the user list for a user with the provided credentials and return their `UserId`, equal to the index in this list.
3. We pass the context with settings (`basicAuthSettings :# HNil`) and a function that handles the result of authentication to the `auth` combinator. Note that the signature `MakeError -> AuthResult Void -> m Void` gives you only the ability to raise an exception, which is recommended to create using the function `makeError :: Http.Status -> Bytes.Lazy.ByteString -> ServerError` (though you're certainly free to throw whatever you'd like).

If you want to add a new authentication method alongside an existing one:
```haskell
data Another

myAuth :: ValueCombinator '[] (WithReq M (Auth '[Basic, Another] UserId)) ts M
myAuth = auth @'[Basic, Another] @UserId (pure $ basicAuthSettings :# HNil) \makeError -> \case
  _ -> do
    liftIO $ putStrLn "Unauthorized!"
    throwM $ makeError (statusOf unauthorized) mempty
```

The compiler will suggest what instance you need to implement:

```haskell
• No instance for ‘IsAuth Another (ReaderT Env IO) UserId’
    arising from a use of ‘auth’
```

Implement the instance:

```haskell
data AnotherAuthSettings = AnotherAuthSettings

instance IsAuth Another m UserId where
  type Settings Another m UserId = AnotherAuthSettings
  toAuth = undefined -- doesn't matter here
```

Then, the compiler will ask you to include settings for this method in the context:

```haskell
• Can't find AnotherAuthSettings in context
• In the expression:
    auth
      @'[Basic, Another] @UserId (pure $ basicAuthSettings :# HNil)
      \ makeError -> \case _ -> do ...
```

Both options `basicAuthSettings :# AnotherAuthSettings :# HNil` and `AnotherAuthSettings :# basicAuthSettings :# HNil` work fine.

Keep in mind that you don't necessarily have to use the `auth` combinator for authentication — you can write your own custom solution.

JWT authentication is available in the `minion-jwt` package.

### Introspection
Since Minion's `Router` is a `GADT`, we can fold it into any desired value. For example, it can be transformed into an OpenAPI 3 schema (this is implemented in the `minion-openapi3` package). All constructors are available from the module `Web.Minion.Router`, while all necessary tools for introspection reside in the module `Web.Minion.Introspect`. You can find an example of writing router introspection in the module `Web.Minion.Examples.Introspection`. Writing custom introspection in Minion is not very difficult, but does require some preparation.

```haskell
data Pretty -- (1)

instance I.HasIntrospection Pretty where -- (2)
  type IntrospectionFor Pretty I.QueryParam = I.AbsolutelyNothing
  type IntrospectionFor Pretty I.Capture = I.AbsolutelyNothing
  type IntrospectionFor Pretty I.Captures = I.AbsolutelyNothing
  type IntrospectionFor Pretty I.Header = I.AbsolutelyNothing
  type IntrospectionFor Pretty I.Request = PrettyBody
  type IntrospectionFor Pretty I.Response = PrettyBody
  type IntrospectionFor Pretty I.Description = PrettyDescription

-- (3)
class PrettyBody a where
  prettyBody :: Text

-- (4)
class PrettyDescription a where
  prettyDescription :: a -> Text
```
1. Firstly, declare your introspection marker type.
2. Implement an instance of the `HasIntrospection` class for this type, specifying which type classes each routing component should implement. In our example, we don't require anything new from `QueryParam`, `Capture`, `Captures`, and `Header`:
   ```haskell
   class AbsolutelyNothing a
   instance AbsolutelyNothing a
   ```
3. Require some textual representation for types that are accepted as requests and returned as responses (`Request` and `Response`).
4. Demand a way to convert `Description` into text.

In this example, we transform the API into some textual representation:
```haskell
prettyApi :: forall i m. (I.Elem Pretty i) => Router' i Void m -> Text
```
When matching on a specific constructor of `Router` to bring the `IntrospectionFor Pretty x` instance into scope, it's important to call the function `withIntrospection` from the `Web.Minion.Introspect` module:
```haskell
case ... of 
  Description @desc d cont -> I.withIntrospection @Pretty @hasPretty @I.Description @desc do
    ...
```
To simplify things, you may define a local helper function like so:
```haskell
wi :: forall t x. (I.Introspection hasPretty t x) 
  => ((I.IntrospectionFor Pretty t x) => [PrettyInfo]) 
  -> [PrettyInfo]
wi = I.withIntrospection @Pretty @hasPretty @t @x
```
Then bringing the required instance will become less verbose:
```haskell
case ... of 
  Description @desc d cont -> wi @I.Description @desc do
    ...
```
Additionally, Minion provides the combinators `hideIntrospection`, allowing parts of the API to be hidden from certain forms of introspection. Suppose you have part of the API that you'd like to exclude from `OpenApi3` but keep accessible via `Pretty` introspection:
```haskell
api :: Router' '[Pretty, Openapi3] Void IO
api = "api" />
  [ hideIntrospection @'[Pretty] $ "only_pretty" /> handleBody @Ok @'[Json] @() GET _
  , "all_introspection" /> handleBody @Ok @'[Json] @() GET _
  ]
```
In such cases, when pattern-matching against `HideIntrospection`, you'll lack proof that the particular introspection you're interested in is available within this subtree. The `Web.Minion.Introspect` module offers the function `withElem`, which attempts to locate evidence that the needed introspection exists in the current subtree, but also requires a fallback value if the introspection isn't found.
```haskell
case ... of
  HideIntrospection @_ @i' rest -> I.withElem @Pretty @i' <empty introspection> <build introspection>
```

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
```
1. A list of introspections, such as OpenApi3 or Client.
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
src/Web/Minion/Examples/HelloWorld.hs: error: [GHC-88464]
    • Found hole: _ :: [Char] -> IO String
    • In the fifth argument of ‘handleBody’, namely ‘_’
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
Note that ValueCombinator is a function that describes how to extract a value of type v from a request.

### Path 
These combinators are defined in the module `Web.Minion.Request.Path` and available from `Web.Minion`.

* `piece` (or string literal with OverloadedStrings enabled): matches a static path segment.
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
    src/Web/Minion/Examples/HelloWorld.hs: error: [GHC-88464]
    • Found hole: _ :: [Char] -> [[Char]] -> IO String
```
1. Implicit `piece` via `OverloadedStrings`
2. Although stylistically it's not very good to mix implicit `piece` through `OverloadedStrings` and explicit one through a function, here it's done for demonstration purposes.
3. Capturing a single path segment
4. Capturing the remainder of the path
5. Handler function, compiler infers its type as `String -> [String] -> IO String`. Thus, the path `/api/internal/john/foo/bar/baz` would invoke the handler with arguments `"john"` and `["foo", "bar", "baz"]`.

Note that `/>` is used after combinators that do not capture new values, while `.>` follows combinators that perform capturing. Internally, each of them is simply `$`, but with more concrete types.

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
src/Web/Minion/Examples/HelloWorld.hs: error: [GHC-88464]
    • Found hole:
        _ :: Int
          -> Maybe Int
          -> Maybe Bool
          -> Maybe (NonEmpty String)
          -> IO [Comment]

src/Web/Minion/Examples/HelloWorld.hs: error: [GHC-88464]
    • Found hole: _ :: CommentsQuery -> IO [Comment]
```
1. Expects a query parameter named `page` of type `Int`.
2. Expects an optional query parameter named `limit` of type `Int`.
3. Expects an optional boolean query flag named `approved`.
4. Captures all query parameters named `tag` into a list.
5. Handler function, compiler infers its type as `Int -> Maybe Int -> Maybe Bool -> Maybe (NonEmpty Int) -> IO [Comment]`.
6. Captures multiple query parameters at once using the `FromForm` parser, which is derived via `Generic`. Note that `queryFlag` is different from `queryParam Bool`, since the former parses `?approved`, `?approved=1`, `?approved=true` as `True`, whereas the latter only accepts `?approved=true`.
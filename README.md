# Minion

Minion is Haskell library for developing web applications. It stands between [Scotty](https://hackage.haskell.org/package/scotty) and [Servant](https://hackage.haskell.org/package/servant-server)  

|                  | Scotty | Minion | Servant |
| ---------------- | ------ | ------ | ------- |
| As simple as ABC | Yes    | No     | No      |
| At term level    | Yes    | Yes    | No      |
| Typesafe         | No     | Yes    | Yes     |
| Introspectable   | No     | Yes    | Yes     |
| Generated client | No     | No     | Yes     |

  
Since Minion defines servers at the term level, it's easier to start and without excess verbosity.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Web.Minion
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = Warp.run 9001 app

app :: ApplicationM IO
app = serve api 

api :: Router Void IO
api = "api" /> 
    [ "about" /> handlePlainText @String GET (pure "Hello-World Minion server")
    , "hello" /> capture @String "name" 
              .> handlePlainText @String GET (\name -> pure $ "Hello, " <> name <> "!")
    ]
```

Documentation and examples can be found on [Hackage](https://hackage.haskell.org/package/minion)  

Minion ecosystem also contains following libraries:
* [minion-conduit](https://hackage.haskell.org/package/minion-conduit) 
* [minion-htmx](https://hackage.haskell.org/package/minion-htmx) 
* [minion-jwt](https://hackage.haskell.org/package/minion-jwt) 
* [minion-wai-extra](https://hackage.haskell.org/package/minion-wai-extra) 
* [minion-openapi3](https://hackage.haskell.org/package/minion-openapi3) 
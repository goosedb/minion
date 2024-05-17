module Web.Minion.Examples.Json (app) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=))
import GHC.Generics
import Web.Minion

app :: ApplicationM IO
app = serve api

data FooRequest = FooRequest
  { foo :: Int
  , bar :: Int
  }
  deriving (Generic, FromJSON)

data Response a = Success a | Failure Error

instance (ToJSON a) => ToJSON (Response a) where
  toJSON (Success a) = object ["success" .= a]
  toJSON (Failure a) = object ["failure" .= a]

data Error = Error
  { code :: String
  , message :: String
  }
  deriving (Generic, ToJSON)

data FooResponse = FooResponse
  { baz :: Int
  , qux :: Int
  }
  deriving (Generic, ToJSON)

api :: Router Void IO
api = "api" /> "json" /> reqJson @FooRequest .> handleJson @(Response FooResponse) POST endpoint
 where
  endpoint FooRequest{..} = pure do
    if foo + bar < 0
      then Failure (Error "you_died" "That's life")
      else Success (FooResponse bar foo)

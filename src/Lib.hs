{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.Cors
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User



startApp :: IO ()
startApp = run 12000 app

app :: Application
app = simpleCors (serve api server2)

api :: Proxy UserAPI2
api = Proxy

server2 :: Server UserAPI2
server2 = return users
      :<|> return albert
      :<|> return isaac

server :: Server API
server = return users


albert :: User
albert = User 1 "Albert" "Einstein"

isaac :: User
isaac = User 2 "Isaac" "Newton"

users :: [User]
users = [albert, isaac]



{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server ( startApp ) where

import Prelude

import Network.Wai.Handler.Warp
import Servant

type UserAPI1
    = Get '[JSON] String
    :<|> "hello" :> Get '[JSON] String

server :: Server UserAPI1
server =
    return "Okay"
    :<|> return "Hello world!"

userAPI :: Proxy UserAPI1
userAPI = Proxy

app :: Application
app = serve userAPI server

startApp :: IO ()
startApp = do
    _ <- putStrLn "Running server on 8090"
    run 8090 app
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server ( startApp ) where

import Types (Dimension(..))
import Data.Text (Text,pack)
import qualified Network.Wai
import Servant (Application, Proxy(..) , Server, serve )
import Servant.API (JSON, Post, (:>) , ReqBody , (:<|>)(..) , Get)
import Network.Wai.Handler.Warp (run)

type UserAPI1 = 
    "dimensions" 
        :> ( "get"  -- dimensions/get
                :> Get '[JSON] [Dimension]
            :<|> "update"
                :> ReqBody '[JSON] Dimension
                :> Post '[JSON] Dimension
            :<|> "create"
                :> ReqBody '[JSON] [Dimension]
                :> Post '[JSON] [Dimension]
           ) 
    :<|> "globalConfig" 
        :> ( "get"  -- globalConfig/get
            :> Get '[JSON] Text
        :<|> "update"
            :> ReqBody '[JSON] Text
            :> Post '[JSON] Text
        :<|> "create"
            :> ReqBody '[JSON] Text
            :> Post '[JSON] Text
            ) 

server :: Server UserAPI1
server = (listAllDimensions :<|> updateDimension :<|> createDimension) :<|> (listAllGlobalConfig :<|> updateGC :<|> createGC)
  where listAllDimensions = pure $ [Dimension "Dimen1234" (Just "tier") (Just "p1") , Dimension "Dimen4567" (Just "zee5") (Just "p1")] -- fetch data from db
        updateDimension dimension =  pure dimension -- needs to be updated in db as well
        createDimension cdimen = pure cdimen
        listAllGlobalConfig = pure $ pack $ "prasanna Globals"
        updateGC gc = pure mempty
        createGC gc = pure mempty

userAPI :: Proxy UserAPI1
userAPI = Proxy

app :: Application
app = serve userAPI server

startApp :: IO ()
startApp = do
    _ <- putStrLn "Running server on 8090"
    run 8090 app
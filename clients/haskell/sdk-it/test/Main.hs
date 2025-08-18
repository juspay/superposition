{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (fold)
import Config qualified
-- import Context qualified
import DefaultConfig qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Dimension qualified
import Io.Superposition.SuperpositionClient qualified as SDK
import Network.HTTP.Client qualified as HTTP
import Network.URI qualified as URI
import Test.Hspec qualified as Test
import Utils
import Prelude

mkClient :: HTTP.Manager -> SDK.SuperpositionClient
mkClient manager = expectRight $ SDK.build $ do
  SDK.setToken "some-token"
  SDK.setEndpointuri $ expectJust $ URI.parseURI "http://localhost:8080"
  SDK.setHttpmanager manager

responseLogMw :: HTTP.Response HTTP.BodyReader -> IO (HTTP.Response HTTP.BodyReader)
responseLogMw resp = do
  putStrLn $ "  Response status: " ++ show (HTTP.responseStatus resp)
  putStrLn $ "  Response headers: " ++ show (HTTP.responseHeaders resp)
  body <- HTTP.brConsume (HTTP.responseBody resp)
  putStrLn $ "  Response body: " ++ show body
  pure resp {HTTP.responseBody = pure $ fold body}

requestToByteString :: HTTP.RequestBody -> IO (Maybe ByteString)
requestToByteString (HTTP.RequestBodyBS bs) = pure $ Just bs
requestToByteString (HTTP.RequestBodyLBS bs) = pure $ Just $ BS.toStrict bs
requestToByteString _ = pure Nothing

requestLogMw :: HTTP.Request -> IO HTTP.Request
requestLogMw req = do
  putStrLn $ "  Request method: " ++ show (HTTP.method req)
  putStrLn $ "  Request headers: " ++ show (HTTP.requestHeaders req)
  body <- requestToByteString $ HTTP.requestBody req
  putStrLn $ "  Request body: " ++ show body
  pure req {HTTP.requestBody = HTTP.RequestBodyBS $ fold body}

managerSettings :: HTTP.ManagerSettings
managerSettings =
  HTTP.defaultManagerSettings
    { HTTP.managerModifyResponse = responseLogMw,
      HTTP.managerModifyRequest = requestLogMw
    }

main :: IO ()
main = do
  manager <- HTTP.newManager managerSettings
  let client = mkClient manager
  Test.hspec $ Test.beforeAll (pure client) $ do
    Dimension.spec
    -- Context.spec
    Config.spec
    DefaultConfig.spec

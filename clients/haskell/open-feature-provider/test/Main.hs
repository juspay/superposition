module Main (main) where

import Data.OpenFeature.Provider qualified as P
import Data.OpenFeature.SuperpositionProvider qualified as P
import GHC.Base (assert)
import GHC.Conc.IO (threadDelay)
import Network.URI qualified as URI

expectJust :: Maybe a -> a
expectJust (Just a) = a
expectJust _ = undefined

expectRight :: Either b a -> a
expectRight (Right a) = a
expectRight _ = undefined

main :: IO ()
main = do
  let options =
        P.defaultProviderOptions
          { P.orgId = "localorg",
            P.workspaceId = "dev",
            P.endpoint = expectJust $ URI.parseURI "http://localhost:8080",
            P.refreshOptions = P.Poll 1,
            P.logLevel = P.LevelDebug
          }
  provider <- expectRight <$> P.newSuperpositionProvider options
  !_ <- expectRight <$> P.initialize provider P.defaultContext
  -- wait a few seconds...
  threadDelay 3000000
  v <- P.resolveBooleanValue provider "bool" False P.defaultContext
  !_ <- pure $ assert (P.value v)
  pure ()

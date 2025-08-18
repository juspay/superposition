module Config(spec) where

import Test.HUnit (assert)
import Test.Hspec (describe, it, SpecWith)
import Io.Superposition.SuperpositionClient qualified as SDK
import Io.Superposition.Command.GetResolvedConfig qualified as CMD
import Io.Superposition.Model.GetResolvedConfigInput qualified as CMD
import Data.Either
import qualified Data.Aeson as Aeson

spec :: SpecWith SDK.SuperpositionClient
spec = describe "Config API" $ do
  it "GetResolvedConfig" getResolvedConfig

getResolvedConfig :: SDK.SuperpositionClient -> IO ()
getResolvedConfig client = do
  out <- CMD.getResolvedConfig client $ do
    CMD.setOrgId "localorg"
    CMD.setWorkspaceId "test"
    CMD.setContext (Just mempty)
  putStrLn $ "getResolvedConfig: " ++ show (Aeson.encode out)
  assert (isRight out)
  pure ()

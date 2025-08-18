module DefaultConfig where

import Data.Aeson qualified as Aeson
import Data.Either
import Data.Text qualified as T
import Io.Superposition.Command.CreateDefaultConfig qualified as CDC
import Io.Superposition.Command.DeleteDefaultConfig qualified as DDC
import Io.Superposition.Command.ListDefaultConfigs qualified as LDC
import Io.Superposition.Command.UpdateDefaultConfig qualified as UDC
import Io.Superposition.Model.CreateDefaultConfigInput qualified as CDC
import Io.Superposition.Model.DeleteDefaultConfigInput qualified as DDC
import Io.Superposition.Model.ListDefaultConfigsInput qualified as LDC
import Io.Superposition.Model.UpdateDefaultConfigInput qualified as UDC
import Io.Superposition.SuperpositionClient qualified as SDK
import Test.HUnit (assert)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Test

spec :: Test.SpecWith SDK.SuperpositionClient
spec = describe "DefaultConfig API" $ do
  it "Create" createDefaultConfig
  it "List" listDefaultConfigs
  it "Update" updateDefaultConfig
  it "Delete" deleteDefaultConfig

createDefaultConfig :: SDK.SuperpositionClient -> IO ()
createDefaultConfig client = do
  putStrLn "Creating default config: testConfig"
  output <- CDC.createDefaultConfig client $ do
    CDC.setOrgId "localorg"
    CDC.setWorkspaceId "test"
    CDC.setKey "testConfig"
    CDC.setValue (Aeson.String "test value")
    CDC.setSchema (Aeson.object ["type" Aeson..= ("string" :: T.Text)])
    CDC.setDescription "Test default config for integration tests"
    CDC.setChangeReason "Integration test"
    CDC.setFunctionName Nothing
    CDC.setAutocompleteFunctionName Nothing
  putStrLn $ "Create default config result: " ++ show output
  assert (isRight output)
  pure ()

listDefaultConfigs :: SDK.SuperpositionClient -> IO ()
listDefaultConfigs client = do
  output <- LDC.listDefaultConfigs client $ do
    LDC.setOrgId "localorg"
    LDC.setWorkspaceId "test"
    LDC.setCount (Just 10)
    LDC.setPage (Just 1)
    LDC.setAll' (Just False)
  putStrLn $ "List default configs result: " ++ show output
  assert (isRight output)
  pure ()

updateDefaultConfig :: SDK.SuperpositionClient -> IO ()
updateDefaultConfig client = do
  putStrLn "Updating default config: testConfig"
  output <- UDC.updateDefaultConfig client $ do
    UDC.setOrgId "localorg"
    UDC.setWorkspaceId "test"
    UDC.setKey "testConfig"
    UDC.setChangeReason "Integration test update"
    UDC.setValue (Just $ Aeson.String "updated test value")
    UDC.setSchema (Just $ Aeson.object ["type" Aeson..= ("string" :: T.Text)])
    UDC.setDescription (Just "Updated test default config")
    UDC.setFunctionName Nothing
    UDC.setAutocompleteFunctionName Nothing
  putStrLn $ "Update default config result: " ++ show output
  assert (isRight output)
  pure ()

deleteDefaultConfig :: SDK.SuperpositionClient -> IO ()
deleteDefaultConfig client = do
  putStrLn "Deleting default config: testConfig"
  output <- DDC.deleteDefaultConfig client $ do
    DDC.setOrgId "localorg"
    DDC.setWorkspaceId "test"
    DDC.setKey "testConfig"
  putStrLn $ "Delete default config result: " ++ show output
  assert (isRight output)
  pure ()

module Dimension where

import Data.Aeson qualified as Aeson
import Data.Either
import Data.Text qualified as T
import Io.Superposition.Command.CreateDimension qualified as CD
import Io.Superposition.Command.DeleteDimension qualified as DD
import Io.Superposition.Command.GetDimension qualified as GD
import Io.Superposition.Command.ListDimensions qualified as LD
import Io.Superposition.Command.UpdateDimension qualified as UD
import Io.Superposition.Model.CreateDimensionInput qualified as CD
import Io.Superposition.Model.DeleteDimensionInput qualified as DD
import Io.Superposition.Model.GetDimensionInput qualified as GD
import Io.Superposition.Model.ListDimensionsInput qualified as LD
import Io.Superposition.Model.UpdateDimensionInput qualified as UD
import Io.Superposition.SuperpositionClient qualified as SDK
import Test.HUnit (assert)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Test

spec :: Test.SpecWith SDK.SuperpositionClient
spec = describe "Dimension API" $ do
  -- it "List" listDimensions
  -- it "Get" getDimension
  it "Create" createDimension
  it "Update" updateDimension
  it "Delete" deleteDimension

listDimensions :: SDK.SuperpositionClient -> IO ()
listDimensions client = do
  output <- LD.listDimensions client $ do
    LD.setOrgId "localorg"
    LD.setWorkspaceId "test"
  putStrLn $ "List dimension result: " ++ show output
  assert (isRight output)
  pure ()

getDimension :: SDK.SuperpositionClient -> IO ()
getDimension client = do
  output <- GD.getDimension client $ do
    GD.setOrgId "localorg"
    GD.setWorkspaceId "test"
    GD.setDimension "variantIds"
  assert (isRight output)
  pure ()

createDimension :: SDK.SuperpositionClient -> IO ()
createDimension client = do
  putStrLn "Creating dimension: testDimension"
  output <- CD.createDimension client $ do
    CD.setOrgId "localorg"
    CD.setWorkspaceId "test"
    CD.setDimension "testDimension"
    CD.setPosition 1
    CD.setSchema (Aeson.object ["type" Aeson..= ("string" :: T.Text)])
    CD.setDescription "Test dimension for integration tests"
    CD.setChangeReason "Integration test"
  putStrLn $ "Create dimension result: " ++ show output
  assert (isRight output)
  pure ()

updateDimension :: SDK.SuperpositionClient -> IO ()
updateDimension client = do
  putStrLn "Updating dimension: testDimension"
  output <- UD.updateDimension client $ do
    UD.setOrgId "localorg"
    UD.setWorkspaceId "test"
    UD.setDimension "testDimension"
    UD.setPosition (Just 1)
    UD.setSchema (Just $ Aeson.object ["type" Aeson..= ("string" :: T.Text)])
    UD.setDescription (Just "Updated test dimension")
    UD.setChangeReason "Integration test update"
  putStrLn $ "Update dimension result: " ++ show output
  assert (isRight output)
  pure ()

deleteDimension :: SDK.SuperpositionClient -> IO ()
deleteDimension client = do
  output <- DD.deleteDimension client $ do
    DD.setOrgId "localorg"
    DD.setWorkspaceId "test"
    DD.setDimension "testDimension"
  putStrLn $ "Delete dimension result: " ++ show output
  assert (isRight output)
  pure ()

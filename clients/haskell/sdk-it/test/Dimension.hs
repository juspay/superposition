module Dimension where

import Data.Either
import Io.Superposition.Command.ListDimensions qualified as CMD
import Io.Superposition.Model.ListDimensionsInput qualified as CMD
import Io.Superposition.SuperpositionClient qualified as SDK
import Test.HUnit (assert)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Test
import Utils

spec = describe "Dimension API" $ do
  it "List" listDimensions

listDimensions :: SDK.SuperpositionClient -> IO ()
listDimensions client = do
  output <- CMD.listDimensions client $ do
    CMD.setOrgId "localorg"
    CMD.setWorkspaceId "test"
  assert (isRight output)
  pure ()

module Io.Superposition.Command.Test (
    TestError (..),
    test
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.FunctionNotFound
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.TestInput
import qualified Io.Superposition.Model.TestOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data TestError =
    FunctionNotFound Io.Superposition.Model.FunctionNotFound.FunctionNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestError
instance Io.Superposition.Utility.OperationError TestError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.FunctionNotFound.FunctionNotFound) = Just (fmap FunctionNotFound (Io.Superposition.Utility.responseParser @Io.Superposition.Model.FunctionNotFound.FunctionNotFound))
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


test :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.TestInput.TestInputBuilder () -> IO (Either TestError Io.Superposition.Model.TestOutput.TestOutput)
test client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.TestInput.build builder)


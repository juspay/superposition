module Io.Superposition.Command.GetFunction (
    GetFunctionError (..),
    getFunction
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.FunctionNotFound
import qualified Io.Superposition.Model.GetFunctionInput
import qualified Io.Superposition.Model.GetFunctionOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data GetFunctionError =
    FunctionNotFound Io.Superposition.Model.FunctionNotFound.FunctionNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON GetFunctionError
instance Io.Superposition.Utility.OperationError GetFunctionError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.FunctionNotFound.FunctionNotFound) = Just (fmap FunctionNotFound (Io.Superposition.Utility.responseParser @Io.Superposition.Model.FunctionNotFound.FunctionNotFound))
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


getFunction :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.GetFunctionInput.GetFunctionInputBuilder () -> IO (Either GetFunctionError Io.Superposition.Model.GetFunctionOutput.GetFunctionOutput)
getFunction client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.GetFunctionInput.build builder)


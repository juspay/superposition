module Io.Superposition.Command.ListDefaultConfigs (
    ListDefaultConfigsError (..),
    listDefaultConfigs
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ListDefaultConfigsInput
import qualified Io.Superposition.Model.ListDefaultConfigsOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data ListDefaultConfigsError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON ListDefaultConfigsError
instance Io.Superposition.Utility.OperationError ListDefaultConfigsError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


listDefaultConfigs :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.ListDefaultConfigsInput.ListDefaultConfigsInputBuilder () -> IO (Either ListDefaultConfigsError Io.Superposition.Model.ListDefaultConfigsOutput.ListDefaultConfigsOutput)
listDefaultConfigs client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.ListDefaultConfigsInput.build builder)


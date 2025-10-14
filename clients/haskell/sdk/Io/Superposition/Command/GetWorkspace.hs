module Io.Superposition.Command.GetWorkspace (
    GetWorkspaceError (..),
    getWorkspace
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.GetWorkspaceInput
import qualified Io.Superposition.Model.GetWorkspaceOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ResourceNotFound
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data GetWorkspaceError =
    ResourceNotFound Io.Superposition.Model.ResourceNotFound.ResourceNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON GetWorkspaceError
instance Io.Superposition.Utility.OperationError GetWorkspaceError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.ResourceNotFound.ResourceNotFound) = Just (fmap ResourceNotFound (Io.Superposition.Utility.responseParser @Io.Superposition.Model.ResourceNotFound.ResourceNotFound))
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


getWorkspace :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.GetWorkspaceInput.GetWorkspaceInputBuilder () -> IO (Either GetWorkspaceError Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput)
getWorkspace client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.GetWorkspaceInput.build builder)


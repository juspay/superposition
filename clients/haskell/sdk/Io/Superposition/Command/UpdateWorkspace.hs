module Io.Superposition.Command.UpdateWorkspace (
    UpdateWorkspaceError (..),
    updateWorkspace
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.UpdateWorkspaceInput
import qualified Io.Superposition.Model.UpdateWorkspaceOutput
import qualified Io.Superposition.Model.WorkspaceNotFound
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data UpdateWorkspaceError =
    WorkspaceNotFound Io.Superposition.Model.WorkspaceNotFound.WorkspaceNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON UpdateWorkspaceError
instance Io.Superposition.Utility.OperationError UpdateWorkspaceError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.WorkspaceNotFound.WorkspaceNotFound) = Just (fmap WorkspaceNotFound (Io.Superposition.Utility.responseParser @Io.Superposition.Model.WorkspaceNotFound.WorkspaceNotFound))
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


updateWorkspace :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.UpdateWorkspaceInput.UpdateWorkspaceInputBuilder () -> IO (Either UpdateWorkspaceError Io.Superposition.Model.UpdateWorkspaceOutput.UpdateWorkspaceOutput)
updateWorkspace client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.UpdateWorkspaceInput.build builder)


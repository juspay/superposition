module Io.Superposition.Command.CreateWorkspace (
    CreateWorkspaceError (..),
    createWorkspace
) where
import qualified Data.Aeson
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.CreateWorkspaceInput
import qualified Io.Superposition.Model.CreateWorkspaceOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data CreateWorkspaceError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Data.Text.Text
    | UnexpectedError Data.Text.Text
    | UnexpectedStatus Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON CreateWorkspaceError
instance Data.Aeson.FromJSON CreateWorkspaceError
instance Io.Superposition.Utility.OperationError CreateWorkspaceError where
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError
    mkUnexpectedStatusError = UnexpectedStatus . Data.Text.pack . show

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


createWorkspace :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInputBuilder () -> IO (Either CreateWorkspaceError Io.Superposition.Model.CreateWorkspaceOutput.CreateWorkspaceOutput)
createWorkspace client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.CreateWorkspaceInput.build builder)


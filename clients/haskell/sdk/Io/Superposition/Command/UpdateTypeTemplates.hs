module Io.Superposition.Command.UpdateTypeTemplates (
    UpdateTypeTemplatesError (..),
    updateTypeTemplates
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.TypeTemplatesNotFound
import qualified Io.Superposition.Model.UpdateTypeTemplatesInput
import qualified Io.Superposition.Model.UpdateTypeTemplatesOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data UpdateTypeTemplatesError =
    TypeTemplatesNotFound Io.Superposition.Model.TypeTemplatesNotFound.TypeTemplatesNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON UpdateTypeTemplatesError
instance Io.Superposition.Utility.OperationError UpdateTypeTemplatesError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.TypeTemplatesNotFound.TypeTemplatesNotFound) = Just (fmap TypeTemplatesNotFound (Io.Superposition.Utility.responseParser @Io.Superposition.Model.TypeTemplatesNotFound.TypeTemplatesNotFound))
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


updateTypeTemplates :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.UpdateTypeTemplatesInput.UpdateTypeTemplatesInputBuilder () -> IO (Either UpdateTypeTemplatesError Io.Superposition.Model.UpdateTypeTemplatesOutput.UpdateTypeTemplatesOutput)
updateTypeTemplates client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.UpdateTypeTemplatesInput.build builder)


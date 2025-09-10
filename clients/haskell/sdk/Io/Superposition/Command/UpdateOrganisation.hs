module Io.Superposition.Command.UpdateOrganisation (
    UpdateOrganisationError (..),
    updateOrganisation
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.OrganisationNotFound
import qualified Io.Superposition.Model.UpdateOrganisationInput
import qualified Io.Superposition.Model.UpdateOrganisationOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data UpdateOrganisationError =
    OrganisationNotFound Io.Superposition.Model.OrganisationNotFound.OrganisationNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON UpdateOrganisationError
instance Io.Superposition.Utility.OperationError UpdateOrganisationError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.OrganisationNotFound.OrganisationNotFound) = Just (fmap OrganisationNotFound (Io.Superposition.Utility.responseParser @Io.Superposition.Model.OrganisationNotFound.OrganisationNotFound))
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


updateOrganisation :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.UpdateOrganisationInput.UpdateOrganisationInputBuilder () -> IO (Either UpdateOrganisationError Io.Superposition.Model.UpdateOrganisationOutput.UpdateOrganisationOutput)
updateOrganisation client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.UpdateOrganisationInput.build builder)


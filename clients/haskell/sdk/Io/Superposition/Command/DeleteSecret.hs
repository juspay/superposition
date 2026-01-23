module Io.Superposition.Command.DeleteSecret (
    DeleteSecretError (..),
    deleteSecret
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.DeleteSecretInput
import qualified Io.Superposition.Model.DeleteSecretOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ResourceNotFound
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data DeleteSecretError =
    ResourceNotFound Io.Superposition.Model.ResourceNotFound.ResourceNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON DeleteSecretError
instance Io.Superposition.Utility.OperationError DeleteSecretError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.ResourceNotFound.ResourceNotFound) = Just (fmap ResourceNotFound (Io.Superposition.Utility.responseParser @Io.Superposition.Model.ResourceNotFound.ResourceNotFound))
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


deleteSecret :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.DeleteSecretInput.DeleteSecretInputBuilder () -> IO (Either DeleteSecretError Io.Superposition.Model.DeleteSecretOutput.DeleteSecretOutput)
deleteSecret client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        auth = Io.Superposition.SuperpositionClient.getAuth client
    in Io.Superposition.Utility.runOperation endpoint manager auth (Io.Superposition.Model.DeleteSecretInput.build builder)


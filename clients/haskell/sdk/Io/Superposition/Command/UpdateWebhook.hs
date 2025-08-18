module Io.Superposition.Command.UpdateWebhook (
    UpdateWebhookError (..),
    updateWebhook
) where
import qualified Data.Aeson
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.UpdateWebhookInput
import qualified Io.Superposition.Model.UpdateWebhookOutput
import qualified Io.Superposition.Model.WebhookNotFound
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data UpdateWebhookError =
    WebhookNotFound Io.Superposition.Model.WebhookNotFound.WebhookNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Data.Text.Text
    | UnexpectedError Data.Text.Text
    | UnexpectedStatus Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON UpdateWebhookError
instance Data.Aeson.FromJSON UpdateWebhookError
instance Io.Superposition.Utility.OperationError UpdateWebhookError where
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError
    mkUnexpectedStatusError = UnexpectedStatus . Data.Text.pack . show

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.WebhookNotFound.WebhookNotFound) = Just (fmap WebhookNotFound (Io.Superposition.Utility.responseParser @Io.Superposition.Model.WebhookNotFound.WebhookNotFound))
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


updateWebhook :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.UpdateWebhookInput.UpdateWebhookInputBuilder () -> IO (Either UpdateWebhookError Io.Superposition.Model.UpdateWebhookOutput.UpdateWebhookOutput)
updateWebhook client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.UpdateWebhookInput.build builder)


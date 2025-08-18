module Io.Superposition.Command.DeleteTypeTemplates (
    DeleteTypeTemplatesError (..),
    deleteTypeTemplates
) where
import qualified Data.Aeson
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.DeleteTypeTemplatesInput
import qualified Io.Superposition.Model.DeleteTypeTemplatesOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.TypeTemplatesNotFound
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data DeleteTypeTemplatesError =
    TypeTemplatesNotFound Io.Superposition.Model.TypeTemplatesNotFound.TypeTemplatesNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Data.Text.Text
    | UnexpectedError Data.Text.Text
    | UnexpectedStatus Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON DeleteTypeTemplatesError
instance Data.Aeson.FromJSON DeleteTypeTemplatesError
instance Io.Superposition.Utility.OperationError DeleteTypeTemplatesError where
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError
    mkUnexpectedStatusError = UnexpectedStatus . Data.Text.pack . show

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.TypeTemplatesNotFound.TypeTemplatesNotFound) = Just (fmap TypeTemplatesNotFound (Io.Superposition.Utility.responseParser @Io.Superposition.Model.TypeTemplatesNotFound.TypeTemplatesNotFound))
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


deleteTypeTemplates :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.DeleteTypeTemplatesInput.DeleteTypeTemplatesInputBuilder () -> IO (Either DeleteTypeTemplatesError Io.Superposition.Model.DeleteTypeTemplatesOutput.DeleteTypeTemplatesOutput)
deleteTypeTemplates client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.DeleteTypeTemplatesInput.build builder)


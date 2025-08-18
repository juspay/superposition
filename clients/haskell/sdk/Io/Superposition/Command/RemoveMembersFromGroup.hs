module Io.Superposition.Command.RemoveMembersFromGroup (
    RemoveMembersFromGroupError (..),
    removeMembersFromGroup
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.RemoveMembersFromGroupInput
import qualified Io.Superposition.Model.RemoveMembersFromGroupOutput
import qualified Io.Superposition.Model.ResourceNotFound
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data RemoveMembersFromGroupError =
    ResourceNotFound Io.Superposition.Model.ResourceNotFound.ResourceNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON RemoveMembersFromGroupError
instance Io.Superposition.Utility.OperationError RemoveMembersFromGroupError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.ResourceNotFound.ResourceNotFound) = Just (fmap ResourceNotFound (Io.Superposition.Utility.responseParser @Io.Superposition.Model.ResourceNotFound.ResourceNotFound))
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


removeMembersFromGroup :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.RemoveMembersFromGroupInput.RemoveMembersFromGroupInputBuilder () -> IO (Either RemoveMembersFromGroupError Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput)
removeMembersFromGroup client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.RemoveMembersFromGroupInput.build builder)


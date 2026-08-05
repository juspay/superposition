module Io.Superposition.Command.ImportConfigJson (
    ImportConfigJsonError (..),
    importConfigJson
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ImportConfigJsonInput
import qualified Io.Superposition.Model.ImportConfigJsonOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data ImportConfigJsonError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON ImportConfigJsonError
instance Io.Superposition.Utility.OperationError ImportConfigJsonError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


importConfigJson :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.ImportConfigJsonInput.ImportConfigJsonInputBuilder () -> IO (Either ImportConfigJsonError Io.Superposition.Model.ImportConfigJsonOutput.ImportConfigJsonOutput)
importConfigJson client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        auth = Io.Superposition.SuperpositionClient.getAuth client
    in Io.Superposition.Utility.runOperation endpoint manager auth (Io.Superposition.Model.ImportConfigJsonInput.build builder)


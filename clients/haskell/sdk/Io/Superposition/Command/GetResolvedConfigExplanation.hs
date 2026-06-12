module Io.Superposition.Command.GetResolvedConfigExplanation (
    GetResolvedConfigExplanationError (..),
    getResolvedConfigExplanation
) where
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.GetResolvedConfigExplanationInput
import qualified Io.Superposition.Model.GetResolvedConfigExplanationOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data GetResolvedConfigExplanationError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Io.Superposition.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Io.Superposition.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON GetResolvedConfigExplanationError
instance Io.Superposition.Utility.OperationError GetResolvedConfigExplanationError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


getResolvedConfigExplanation :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.GetResolvedConfigExplanationInput.GetResolvedConfigExplanationInputBuilder () -> IO (Either GetResolvedConfigExplanationError Io.Superposition.Model.GetResolvedConfigExplanationOutput.GetResolvedConfigExplanationOutput)
getResolvedConfigExplanation client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        auth = Io.Superposition.SuperpositionClient.getAuth client
    in Io.Superposition.Utility.runOperation endpoint manager auth (Io.Superposition.Model.GetResolvedConfigExplanationInput.build builder)


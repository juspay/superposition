module Io.Superposition.Command.ListDimensions (
    ListDimensionsError (..),
    listDimensions
) where
import qualified Data.Aeson
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ListDimensionsInput
import qualified Io.Superposition.Model.ListDimensionsOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility

data ListDimensionsError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Data.Text.Text
    | UnexpectedError Data.Text.Text
    | UnexpectedStatus Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON ListDimensionsError
instance Data.Aeson.FromJSON ListDimensionsError
instance Io.Superposition.Utility.OperationError ListDimensionsError where
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError
    mkUnexpectedStatusError = UnexpectedStatus . Data.Text.pack . show

    getErrorParser status
        | status == (Io.Superposition.Utility.expectedStatus @Io.Superposition.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Io.Superposition.Utility.responseParser @Io.Superposition.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


listDimensions :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.ListDimensionsInput.ListDimensionsInputBuilder () -> IO (Either ListDimensionsError Io.Superposition.Model.ListDimensionsOutput.ListDimensionsOutput)
listDimensions client builder =
    let endpoint = Io.Superposition.SuperpositionClient.endpointUri client
        manager = Io.Superposition.SuperpositionClient.httpManager client
        token = Io.Superposition.SuperpositionClient.token client
        setAuth = Io.Superposition.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Io.Superposition.Utility.runOperation endpoint manager setAuth (Io.Superposition.Model.ListDimensionsInput.build builder)


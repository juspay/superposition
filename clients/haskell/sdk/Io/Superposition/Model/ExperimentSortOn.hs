module Io.Superposition.Model.ExperimentSortOn (
    ExperimentSortOn(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for ExperimentSortOn
data ExperimentSortOn =
    LastModifiedAt
    | CreatedAt
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ExperimentSortOn where
    toJSON LastModifiedAt = Data.Aeson.String $ Data.Text.pack "last_modified_at"
    toJSON CreatedAt = Data.Aeson.String $ Data.Text.pack "created_at"

instance Io.Superposition.Utility.RequestSegment ExperimentSortOn where
    toRequestSegment LastModifiedAt = "last_modified_at"
    toRequestSegment CreatedAt = "created_at"

instance Data.Aeson.FromJSON ExperimentSortOn where
    parseJSON = Data.Aeson.withText "ExperimentSortOn" $ \v ->
        case v of
            "last_modified_at" -> pure LastModifiedAt
            "created_at" -> pure CreatedAt
            _ -> fail $ "Unknown value for ExperimentSortOn: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment ExperimentSortOn where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "last_modified_at" -> Data.Either.Right LastModifiedAt
        Data.Either.Right "created_at" -> Data.Either.Right CreatedAt
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



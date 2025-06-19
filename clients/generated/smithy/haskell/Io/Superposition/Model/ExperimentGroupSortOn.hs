module Io.Superposition.Model.ExperimentGroupSortOn (
    ExperimentGroupSortOn(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for ExperimentGroupSortOn
data ExperimentGroupSortOn =
    Name
    | CreatedAt
    | LastModifiedAt
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ExperimentGroupSortOn where
    toJSON Name = Data.Aeson.String $ Data.Text.pack "name"
    toJSON CreatedAt = Data.Aeson.String $ Data.Text.pack "created_at"
    toJSON LastModifiedAt = Data.Aeson.String $ Data.Text.pack "last_modified_at"

instance Io.Superposition.Utility.RequestSegment ExperimentGroupSortOn where
    toRequestSegment Name = "name"
    toRequestSegment CreatedAt = "created_at"
    toRequestSegment LastModifiedAt = "last_modified_at"

instance Data.Aeson.FromJSON ExperimentGroupSortOn where
    parseJSON = Data.Aeson.withText "ExperimentGroupSortOn" $ \v ->
        case v of
            "name" -> pure Name
            "created_at" -> pure CreatedAt
            "last_modified_at" -> pure LastModifiedAt
            _ -> fail $ "Unknown value for ExperimentGroupSortOn: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment ExperimentGroupSortOn where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "name" -> Data.Either.Right Name
        Data.Either.Right "created_at" -> Data.Either.Right CreatedAt
        Data.Either.Right "last_modified_at" -> Data.Either.Right LastModifiedAt
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



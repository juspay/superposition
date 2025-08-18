module Io.Superposition.Model.ExperimentGroupSortOn (
    ExperimentGroupSortOn(..)
) where
import qualified Data.Aeson
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

instance Data.Aeson.FromJSON ExperimentGroupSortOn where
    parseJSON = Data.Aeson.withText "ExperimentGroupSortOn" $ \v ->
        case v of
            "name" -> pure Name
            "created_at" -> pure CreatedAt
            "last_modified_at" -> pure LastModifiedAt
            _ -> fail $ "Unknown value for ExperimentGroupSortOn: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe ExperimentGroupSortOn where
    serializeElement Name = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "name"
    serializeElement CreatedAt = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "created_at"
    serializeElement LastModifiedAt = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "last_modified_at"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "name" -> Right Name
        "created_at" -> Right CreatedAt
        "last_modified_at" -> Right LastModifiedAt
        e -> Left ("Failed to de-serialize ExperimentGroupSortOn, encountered unknown variant: " ++ (show bs))
    



module Io.Superposition.Model.ExperimentSortOn (
    ExperimentSortOn(..)
) where
import qualified Data.Aeson
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

instance Data.Aeson.FromJSON ExperimentSortOn where
    parseJSON = Data.Aeson.withText "ExperimentSortOn" $ \v ->
        case v of
            "last_modified_at" -> pure LastModifiedAt
            "created_at" -> pure CreatedAt
            _ -> fail $ "Unknown value for ExperimentSortOn: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe ExperimentSortOn where
    serializeElement LastModifiedAt = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "last_modified_at"
    serializeElement CreatedAt = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "created_at"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "last_modified_at" -> Right LastModifiedAt
        "created_at" -> Right CreatedAt
        e -> Left ("Failed to de-serialize ExperimentSortOn, encountered unknown variant: " ++ (show bs))
    



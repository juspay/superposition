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
    LAST_MODIFIED_AT
    | CREATED_AT
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ExperimentSortOn where
    toJSON LAST_MODIFIED_AT = Data.Aeson.String $ Data.Text.pack "last_modified_at"
    toJSON CREATED_AT = Data.Aeson.String $ Data.Text.pack "created_at"

instance Data.Aeson.FromJSON ExperimentSortOn where
    parseJSON = Data.Aeson.withText "ExperimentSortOn" $ \v ->
        case v of
            "last_modified_at" -> pure LAST_MODIFIED_AT
            "created_at" -> pure CREATED_AT
            _ -> fail $ "Unknown value for ExperimentSortOn: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe ExperimentSortOn where
    serializeElement LAST_MODIFIED_AT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "last_modified_at"
    serializeElement CREATED_AT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "created_at"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "last_modified_at" -> Right LAST_MODIFIED_AT
        "created_at" -> Right CREATED_AT
        e -> Left ("Failed to de-serialize ExperimentSortOn, encountered unknown variant: " ++ (show bs))
    



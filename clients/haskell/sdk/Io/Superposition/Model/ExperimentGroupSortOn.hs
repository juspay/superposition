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
    NAME
    | CREATED_AT
    | LAST_MODIFIED_AT
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ExperimentGroupSortOn where
    toJSON NAME = Data.Aeson.String $ Data.Text.pack "name"
    toJSON CREATED_AT = Data.Aeson.String $ Data.Text.pack "created_at"
    toJSON LAST_MODIFIED_AT = Data.Aeson.String $ Data.Text.pack "last_modified_at"

instance Data.Aeson.FromJSON ExperimentGroupSortOn where
    parseJSON = Data.Aeson.withText "ExperimentGroupSortOn" $ \v ->
        case v of
            "name" -> pure NAME
            "created_at" -> pure CREATED_AT
            "last_modified_at" -> pure LAST_MODIFIED_AT
            _ -> fail $ "Unknown value for ExperimentGroupSortOn: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe ExperimentGroupSortOn where
    serializeElement NAME = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "name"
    serializeElement CREATED_AT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "created_at"
    serializeElement LAST_MODIFIED_AT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "last_modified_at"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "name" -> Right NAME
        "created_at" -> Right CREATED_AT
        "last_modified_at" -> Right LAST_MODIFIED_AT
        e -> Left ("Failed to de-serialize ExperimentGroupSortOn, encountered unknown variant: " ++ (show bs))
    



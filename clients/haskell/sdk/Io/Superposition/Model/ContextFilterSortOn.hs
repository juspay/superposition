module Io.Superposition.Model.ContextFilterSortOn (
    ContextFilterSortOn(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for ContextFilterSortOn
data ContextFilterSortOn =
    LAST_MODIFIED_AT
    | CREATED_AT
    | WEIGHT
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ContextFilterSortOn where
    toJSON LAST_MODIFIED_AT = Data.Aeson.String $ Data.Text.pack "last_modified_at"
    toJSON CREATED_AT = Data.Aeson.String $ Data.Text.pack "created_at"
    toJSON WEIGHT = Data.Aeson.String $ Data.Text.pack "weight"

instance Data.Aeson.FromJSON ContextFilterSortOn where
    parseJSON = Data.Aeson.withText "ContextFilterSortOn" $ \v ->
        case v of
            "last_modified_at" -> pure LAST_MODIFIED_AT
            "created_at" -> pure CREATED_AT
            "weight" -> pure WEIGHT
            _ -> fail $ "Unknown value for ContextFilterSortOn: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe ContextFilterSortOn where
    serializeElement LAST_MODIFIED_AT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "last_modified_at"
    serializeElement CREATED_AT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "created_at"
    serializeElement WEIGHT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "weight"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "last_modified_at" -> Right LAST_MODIFIED_AT
        "created_at" -> Right CREATED_AT
        "weight" -> Right WEIGHT
        e -> Left ("Failed to de-serialize ContextFilterSortOn, encountered unknown variant: " ++ (show bs))
    



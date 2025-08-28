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
    LastModifiedAt
    | CreatedAt
    | Weight
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ContextFilterSortOn where
    toJSON LastModifiedAt = Data.Aeson.String $ Data.Text.pack "last_modified_at"
    toJSON CreatedAt = Data.Aeson.String $ Data.Text.pack "created_at"
    toJSON Weight = Data.Aeson.String $ Data.Text.pack "weight"

instance Data.Aeson.FromJSON ContextFilterSortOn where
    parseJSON = Data.Aeson.withText "ContextFilterSortOn" $ \v ->
        case v of
            "last_modified_at" -> pure LastModifiedAt
            "created_at" -> pure CreatedAt
            "weight" -> pure Weight
            _ -> fail $ "Unknown value for ContextFilterSortOn: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe ContextFilterSortOn where
    serializeElement LastModifiedAt = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "last_modified_at"
    serializeElement CreatedAt = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "created_at"
    serializeElement Weight = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "weight"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "last_modified_at" -> Right LastModifiedAt
        "created_at" -> Right CreatedAt
        "weight" -> Right Weight
        e -> Left ("Failed to de-serialize ContextFilterSortOn, encountered unknown variant: " ++ (show bs))
    



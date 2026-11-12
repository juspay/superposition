module Io.Superposition.Model.DefaultConfigSortOn (
    DefaultConfigSortOn(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for DefaultConfigSortOn
data DefaultConfigSortOn =
    KEY
    | CREATED_AT
    | LAST_MODIFIED_AT
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON DefaultConfigSortOn where
    toJSON KEY = Data.Aeson.String $ Data.Text.pack "key"
    toJSON CREATED_AT = Data.Aeson.String $ Data.Text.pack "created_at"
    toJSON LAST_MODIFIED_AT = Data.Aeson.String $ Data.Text.pack "last_modified_at"

instance Data.Aeson.FromJSON DefaultConfigSortOn where
    parseJSON = Data.Aeson.withText "DefaultConfigSortOn" $ \v ->
        case v of
            "key" -> pure KEY
            "created_at" -> pure CREATED_AT
            "last_modified_at" -> pure LAST_MODIFIED_AT
            _ -> fail $ "Unknown value for DefaultConfigSortOn: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe DefaultConfigSortOn where
    serializeElement KEY = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "key"
    serializeElement CREATED_AT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "created_at"
    serializeElement LAST_MODIFIED_AT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "last_modified_at"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "key" -> Right KEY
        "created_at" -> Right CREATED_AT
        "last_modified_at" -> Right LAST_MODIFIED_AT
        e -> Left ("Failed to de-serialize DefaultConfigSortOn, encountered unknown variant: " ++ (show bs))
    



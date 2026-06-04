module Io.Superposition.Model.ImportMode (
    ImportMode(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for ImportMode
data ImportMode =
    MERGE
    | REPLACE
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ImportMode where
    toJSON MERGE = Data.Aeson.String $ Data.Text.pack "merge"
    toJSON REPLACE = Data.Aeson.String $ Data.Text.pack "replace"

instance Data.Aeson.FromJSON ImportMode where
    parseJSON = Data.Aeson.withText "ImportMode" $ \v ->
        case v of
            "merge" -> pure MERGE
            "replace" -> pure REPLACE
            _ -> fail $ "Unknown value for ImportMode: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe ImportMode where
    serializeElement MERGE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "merge"
    serializeElement REPLACE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "replace"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "merge" -> Right MERGE
        "replace" -> Right REPLACE
        e -> Left ("Failed to de-serialize ImportMode, encountered unknown variant: " ++ (show bs))
    



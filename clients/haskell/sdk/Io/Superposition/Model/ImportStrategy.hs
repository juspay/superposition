module Io.Superposition.Model.ImportStrategy (
    ImportStrategy(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for ImportStrategy
data ImportStrategy =
    CREATE_ONLY
    | UPSERT
    | REPLACE
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ImportStrategy where
    toJSON CREATE_ONLY = Data.Aeson.String $ Data.Text.pack "create_only"
    toJSON UPSERT = Data.Aeson.String $ Data.Text.pack "upsert"
    toJSON REPLACE = Data.Aeson.String $ Data.Text.pack "replace"

instance Data.Aeson.FromJSON ImportStrategy where
    parseJSON = Data.Aeson.withText "ImportStrategy" $ \v ->
        case v of
            "create_only" -> pure CREATE_ONLY
            "upsert" -> pure UPSERT
            "replace" -> pure REPLACE
            _ -> fail $ "Unknown value for ImportStrategy: " <> Data.Text.unpack v
instance Io.Superposition.Utility.SerDe ImportStrategy where
    serializeElement CREATE_ONLY = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "create_only"
    serializeElement UPSERT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "upsert"
    serializeElement REPLACE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "replace"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "create_only" -> Right CREATE_ONLY
        "upsert" -> Right UPSERT
        "replace" -> Right REPLACE
        e -> Left ("Failed to de-serialize ImportStrategy, encountered unknown variant: " ++ (show bs))

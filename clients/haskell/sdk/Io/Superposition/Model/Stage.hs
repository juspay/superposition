module Io.Superposition.Model.Stage (
    Stage(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for Stage
data Stage =
    DRAFT
    | PUBLISHED
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON Stage where
    toJSON DRAFT = Data.Aeson.String $ Data.Text.pack "draft"
    toJSON PUBLISHED = Data.Aeson.String $ Data.Text.pack "published"

instance Data.Aeson.FromJSON Stage where
    parseJSON = Data.Aeson.withText "Stage" $ \v ->
        case v of
            "draft" -> pure DRAFT
            "published" -> pure PUBLISHED
            _ -> fail $ "Unknown value for Stage: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe Stage where
    serializeElement DRAFT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "draft"
    serializeElement PUBLISHED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "published"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "draft" -> Right DRAFT
        "published" -> Right PUBLISHED
        e -> Left ("Failed to de-serialize Stage, encountered unknown variant: " ++ (show bs))
    



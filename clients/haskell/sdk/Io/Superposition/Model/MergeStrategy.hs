module Io.Superposition.Model.MergeStrategy (
    MergeStrategy(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for MergeStrategy
data MergeStrategy =
    MERGE
    | REPLACE
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON MergeStrategy where
    toJSON MERGE = Data.Aeson.String $ Data.Text.pack "MERGE"
    toJSON REPLACE = Data.Aeson.String $ Data.Text.pack "REPLACE"

instance Data.Aeson.FromJSON MergeStrategy where
    parseJSON = Data.Aeson.withText "MergeStrategy" $ \v ->
        case v of
            "MERGE" -> pure MERGE
            "REPLACE" -> pure REPLACE
            _ -> fail $ "Unknown value for MergeStrategy: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe MergeStrategy where
    serializeElement MERGE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "MERGE"
    serializeElement REPLACE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "REPLACE"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "MERGE" -> Right MERGE
        "REPLACE" -> Right REPLACE
        e -> Left ("Failed to de-serialize MergeStrategy, encountered unknown variant: " ++ (show bs))
    



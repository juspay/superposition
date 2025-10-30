module Io.Superposition.Model.DimensionMatchStrategy (
    DimensionMatchStrategy(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for DimensionMatchStrategy
data DimensionMatchStrategy =
    EXACT
    | SUBSET
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON DimensionMatchStrategy where
    toJSON EXACT = Data.Aeson.String $ Data.Text.pack "exact"
    toJSON SUBSET = Data.Aeson.String $ Data.Text.pack "subset"

instance Data.Aeson.FromJSON DimensionMatchStrategy where
    parseJSON = Data.Aeson.withText "DimensionMatchStrategy" $ \v ->
        case v of
            "exact" -> pure EXACT
            "subset" -> pure SUBSET
            _ -> fail $ "Unknown value for DimensionMatchStrategy: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe DimensionMatchStrategy where
    serializeElement EXACT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "exact"
    serializeElement SUBSET = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "subset"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "exact" -> Right EXACT
        "subset" -> Right SUBSET
        e -> Left ("Failed to de-serialize DimensionMatchStrategy, encountered unknown variant: " ++ (show bs))
    



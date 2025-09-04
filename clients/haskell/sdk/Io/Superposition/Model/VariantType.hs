module Io.Superposition.Model.VariantType (
    VariantType(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for VariantType
data VariantType =
    CONTROL
    | EXPERIMENTAL
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON VariantType where
    toJSON CONTROL = Data.Aeson.String $ Data.Text.pack "CONTROL"
    toJSON EXPERIMENTAL = Data.Aeson.String $ Data.Text.pack "EXPERIMENTAL"

instance Data.Aeson.FromJSON VariantType where
    parseJSON = Data.Aeson.withText "VariantType" $ \v ->
        case v of
            "CONTROL" -> pure CONTROL
            "EXPERIMENTAL" -> pure EXPERIMENTAL
            _ -> fail $ "Unknown value for VariantType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe VariantType where
    serializeElement CONTROL = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "CONTROL"
    serializeElement EXPERIMENTAL = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "EXPERIMENTAL"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "CONTROL" -> Right CONTROL
        "EXPERIMENTAL" -> Right EXPERIMENTAL
        e -> Left ("Failed to de-serialize VariantType, encountered unknown variant: " ++ (show bs))
    



module Io.Superposition.Model.FunctionTypes (
    FunctionTypes(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for FunctionTypes
data FunctionTypes =
    VALIDATION
    | AUTOCOMPLETE
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON FunctionTypes where
    toJSON VALIDATION = Data.Aeson.String $ Data.Text.pack "VALIDATION"
    toJSON AUTOCOMPLETE = Data.Aeson.String $ Data.Text.pack "AUTOCOMPLETE"

instance Data.Aeson.FromJSON FunctionTypes where
    parseJSON = Data.Aeson.withText "FunctionTypes" $ \v ->
        case v of
            "VALIDATION" -> pure VALIDATION
            "AUTOCOMPLETE" -> pure AUTOCOMPLETE
            _ -> fail $ "Unknown value for FunctionTypes: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe FunctionTypes where
    serializeElement VALIDATION = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "VALIDATION"
    serializeElement AUTOCOMPLETE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "AUTOCOMPLETE"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "VALIDATION" -> Right VALIDATION
        "AUTOCOMPLETE" -> Right AUTOCOMPLETE
        e -> Left ("Failed to de-serialize FunctionTypes, encountered unknown variant: " ++ (show bs))
    



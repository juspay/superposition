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
    Validation
    | Autocomplete
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON FunctionTypes where
    toJSON Validation = Data.Aeson.String $ Data.Text.pack "VALIDATION"
    toJSON Autocomplete = Data.Aeson.String $ Data.Text.pack "AUTOCOMPLETE"

instance Data.Aeson.FromJSON FunctionTypes where
    parseJSON = Data.Aeson.withText "FunctionTypes" $ \v ->
        case v of
            "VALIDATION" -> pure Validation
            "AUTOCOMPLETE" -> pure Autocomplete
            _ -> fail $ "Unknown value for FunctionTypes: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe FunctionTypes where
    serializeElement Validation = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "VALIDATION"
    serializeElement Autocomplete = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "AUTOCOMPLETE"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "VALIDATION" -> Right Validation
        "AUTOCOMPLETE" -> Right Autocomplete
        e -> Left ("Failed to de-serialize FunctionTypes, encountered unknown variant: " ++ (show bs))
    



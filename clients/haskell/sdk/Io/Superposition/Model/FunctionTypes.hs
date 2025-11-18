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
    VALUE_VALIDATION
    | VALUE_COMPUTE
    | CONTEXT_VALIDATION
    | CHANGE_REASON_VALIDATION
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON FunctionTypes where
    toJSON VALUE_VALIDATION = Data.Aeson.String $ Data.Text.pack "VALUE_VALIDATION"
    toJSON VALUE_COMPUTE = Data.Aeson.String $ Data.Text.pack "VALUE_COMPUTE"
    toJSON CONTEXT_VALIDATION = Data.Aeson.String $ Data.Text.pack "CONTEXT_VALIDATION"
    toJSON CHANGE_REASON_VALIDATION = Data.Aeson.String $ Data.Text.pack "CHANGE_REASON_VALIDATION"

instance Data.Aeson.FromJSON FunctionTypes where
    parseJSON = Data.Aeson.withText "FunctionTypes" $ \v ->
        case v of
            "VALUE_VALIDATION" -> pure VALUE_VALIDATION
            "VALUE_COMPUTE" -> pure VALUE_COMPUTE
            "CONTEXT_VALIDATION" -> pure CONTEXT_VALIDATION
            "CHANGE_REASON_VALIDATION" -> pure CHANGE_REASON_VALIDATION
            _ -> fail $ "Unknown value for FunctionTypes: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe FunctionTypes where
    serializeElement VALUE_VALIDATION = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "VALUE_VALIDATION"
    serializeElement VALUE_COMPUTE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "VALUE_COMPUTE"
    serializeElement CONTEXT_VALIDATION = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "CONTEXT_VALIDATION"
    serializeElement CHANGE_REASON_VALIDATION = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "CHANGE_REASON_VALIDATION"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "VALUE_VALIDATION" -> Right VALUE_VALIDATION
        "VALUE_COMPUTE" -> Right VALUE_COMPUTE
        "CONTEXT_VALIDATION" -> Right CONTEXT_VALIDATION
        "CHANGE_REASON_VALIDATION" -> Right CHANGE_REASON_VALIDATION
        e -> Left ("Failed to de-serialize FunctionTypes, encountered unknown variant: " ++ (show bs))
    



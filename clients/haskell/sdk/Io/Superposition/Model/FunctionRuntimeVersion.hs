module Io.Superposition.Model.FunctionRuntimeVersion (
    FunctionRuntimeVersion(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for FunctionRuntimeVersion
data FunctionRuntimeVersion =
    V1
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON FunctionRuntimeVersion where
    toJSON V1 = Data.Aeson.String $ Data.Text.pack "1.0"

instance Data.Aeson.FromJSON FunctionRuntimeVersion where
    parseJSON = Data.Aeson.withText "FunctionRuntimeVersion" $ \v ->
        case v of
            "1.0" -> pure V1
            _ -> fail $ "Unknown value for FunctionRuntimeVersion: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe FunctionRuntimeVersion where
    serializeElement V1 = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "1.0"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "1.0" -> Right V1
        e -> Left ("Failed to de-serialize FunctionRuntimeVersion, encountered unknown variant: " ++ (show bs))
    



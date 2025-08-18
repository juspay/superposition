module Io.Superposition.Model.Version (
    Version(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for Version
data Version =
    V1
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON Version where
    toJSON V1 = Data.Aeson.String $ Data.Text.pack "V1"

instance Data.Aeson.FromJSON Version where
    parseJSON = Data.Aeson.withText "Version" $ \v ->
        case v of
            "V1" -> pure V1
            _ -> fail $ "Unknown value for Version: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe Version where
    serializeElement V1 = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "V1"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "V1" -> Right V1
        e -> Left ("Failed to de-serialize Version, encountered unknown variant: " ++ (show bs))
    



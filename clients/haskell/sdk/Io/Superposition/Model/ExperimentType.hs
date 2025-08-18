module Io.Superposition.Model.ExperimentType (
    ExperimentType(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for ExperimentType
data ExperimentType =
    DEFAULT'
    | DELETE_OVERRIDES
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ExperimentType where
    toJSON DEFAULT' = Data.Aeson.String $ Data.Text.pack "DEFAULT"
    toJSON DELETE_OVERRIDES = Data.Aeson.String $ Data.Text.pack "DELETE_OVERRIDES"

instance Data.Aeson.FromJSON ExperimentType where
    parseJSON = Data.Aeson.withText "ExperimentType" $ \v ->
        case v of
            "DEFAULT" -> pure DEFAULT'
            "DELETE_OVERRIDES" -> pure DELETE_OVERRIDES
            _ -> fail $ "Unknown value for ExperimentType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe ExperimentType where
    serializeElement DEFAULT' = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "DEFAULT"
    serializeElement DELETE_OVERRIDES = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "DELETE_OVERRIDES"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "DEFAULT" -> Right DEFAULT'
        "DELETE_OVERRIDES" -> Right DELETE_OVERRIDES
        e -> Left ("Failed to de-serialize ExperimentType, encountered unknown variant: " ++ (show bs))
    



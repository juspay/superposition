module Io.Superposition.Model.ExperimentStatusType (
    ExperimentStatusType(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for ExperimentStatusType
data ExperimentStatusType =
    CREATED
    | CONCLUDED
    | INPROGRESS
    | DISCARDED
    | PAUSED
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ExperimentStatusType where
    toJSON CREATED = Data.Aeson.String $ Data.Text.pack "CREATED"
    toJSON CONCLUDED = Data.Aeson.String $ Data.Text.pack "CONCLUDED"
    toJSON INPROGRESS = Data.Aeson.String $ Data.Text.pack "INPROGRESS"
    toJSON DISCARDED = Data.Aeson.String $ Data.Text.pack "DISCARDED"
    toJSON PAUSED = Data.Aeson.String $ Data.Text.pack "PAUSED"

instance Data.Aeson.FromJSON ExperimentStatusType where
    parseJSON = Data.Aeson.withText "ExperimentStatusType" $ \v ->
        case v of
            "CREATED" -> pure CREATED
            "CONCLUDED" -> pure CONCLUDED
            "INPROGRESS" -> pure INPROGRESS
            "DISCARDED" -> pure DISCARDED
            "PAUSED" -> pure PAUSED
            _ -> fail $ "Unknown value for ExperimentStatusType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe ExperimentStatusType where
    serializeElement CREATED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "CREATED"
    serializeElement CONCLUDED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "CONCLUDED"
    serializeElement INPROGRESS = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "INPROGRESS"
    serializeElement DISCARDED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "DISCARDED"
    serializeElement PAUSED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "PAUSED"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "CREATED" -> Right CREATED
        "CONCLUDED" -> Right CONCLUDED
        "INPROGRESS" -> Right INPROGRESS
        "DISCARDED" -> Right DISCARDED
        "PAUSED" -> Right PAUSED
        e -> Left ("Failed to de-serialize ExperimentStatusType, encountered unknown variant: " ++ (show bs))
    



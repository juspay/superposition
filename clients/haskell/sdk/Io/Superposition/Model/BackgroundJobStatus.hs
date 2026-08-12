module Io.Superposition.Model.BackgroundJobStatus (
    BackgroundJobStatus(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for BackgroundJobStatus
data BackgroundJobStatus =
    CREATED
    | SCHEDULED
    | INPROGRESS
    | FAILED
    | COMPLETED
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON BackgroundJobStatus where
    toJSON CREATED = Data.Aeson.String $ Data.Text.pack "CREATED"
    toJSON SCHEDULED = Data.Aeson.String $ Data.Text.pack "SCHEDULED"
    toJSON INPROGRESS = Data.Aeson.String $ Data.Text.pack "INPROGRESS"
    toJSON FAILED = Data.Aeson.String $ Data.Text.pack "FAILED"
    toJSON COMPLETED = Data.Aeson.String $ Data.Text.pack "COMPLETED"

instance Data.Aeson.FromJSON BackgroundJobStatus where
    parseJSON = Data.Aeson.withText "BackgroundJobStatus" $ \v ->
        case v of
            "CREATED" -> pure CREATED
            "SCHEDULED" -> pure SCHEDULED
            "INPROGRESS" -> pure INPROGRESS
            "FAILED" -> pure FAILED
            "COMPLETED" -> pure COMPLETED
            _ -> fail $ "Unknown value for BackgroundJobStatus: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe BackgroundJobStatus where
    serializeElement CREATED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "CREATED"
    serializeElement SCHEDULED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "SCHEDULED"
    serializeElement INPROGRESS = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "INPROGRESS"
    serializeElement FAILED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "FAILED"
    serializeElement COMPLETED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "COMPLETED"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "CREATED" -> Right CREATED
        "SCHEDULED" -> Right SCHEDULED
        "INPROGRESS" -> Right INPROGRESS
        "FAILED" -> Right FAILED
        "COMPLETED" -> Right COMPLETED
        e -> Left ("Failed to de-serialize BackgroundJobStatus, encountered unknown variant: " ++ (show bs))
    



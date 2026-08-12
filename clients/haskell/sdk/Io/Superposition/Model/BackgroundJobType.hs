module Io.Superposition.Model.BackgroundJobType (
    BackgroundJobType(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for BackgroundJobType
data BackgroundJobType =
    WEBHOOK
    | PRIORITY_RECOMPUTE
    | REDUCE
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON BackgroundJobType where
    toJSON WEBHOOK = Data.Aeson.String $ Data.Text.pack "WEBHOOK"
    toJSON PRIORITY_RECOMPUTE = Data.Aeson.String $ Data.Text.pack "PRIORITY_RECOMPUTE"
    toJSON REDUCE = Data.Aeson.String $ Data.Text.pack "REDUCE"

instance Data.Aeson.FromJSON BackgroundJobType where
    parseJSON = Data.Aeson.withText "BackgroundJobType" $ \v ->
        case v of
            "WEBHOOK" -> pure WEBHOOK
            "PRIORITY_RECOMPUTE" -> pure PRIORITY_RECOMPUTE
            "REDUCE" -> pure REDUCE
            _ -> fail $ "Unknown value for BackgroundJobType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe BackgroundJobType where
    serializeElement WEBHOOK = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "WEBHOOK"
    serializeElement PRIORITY_RECOMPUTE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "PRIORITY_RECOMPUTE"
    serializeElement REDUCE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "REDUCE"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "WEBHOOK" -> Right WEBHOOK
        "PRIORITY_RECOMPUTE" -> Right PRIORITY_RECOMPUTE
        "REDUCE" -> Right REDUCE
        e -> Left ("Failed to de-serialize BackgroundJobType, encountered unknown variant: " ++ (show bs))
    



module Io.Superposition.Model.WorkspaceStatus (
    WorkspaceStatus(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for WorkspaceStatus
data WorkspaceStatus =
    ENABLED
    | DISABLED
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON WorkspaceStatus where
    toJSON ENABLED = Data.Aeson.String $ Data.Text.pack "ENABLED"
    toJSON DISABLED = Data.Aeson.String $ Data.Text.pack "DISABLED"

instance Data.Aeson.FromJSON WorkspaceStatus where
    parseJSON = Data.Aeson.withText "WorkspaceStatus" $ \v ->
        case v of
            "ENABLED" -> pure ENABLED
            "DISABLED" -> pure DISABLED
            _ -> fail $ "Unknown value for WorkspaceStatus: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe WorkspaceStatus where
    serializeElement ENABLED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "ENABLED"
    serializeElement DISABLED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "DISABLED"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "ENABLED" -> Right ENABLED
        "DISABLED" -> Right DISABLED
        e -> Left ("Failed to de-serialize WorkspaceStatus, encountered unknown variant: " ++ (show bs))
    



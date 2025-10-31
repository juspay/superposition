module Io.Superposition.Model.AuditAction (
    AuditAction(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for AuditAction
data AuditAction =
    INSERT
    | UPDATE
    | DELETE
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON AuditAction where
    toJSON INSERT = Data.Aeson.String $ Data.Text.pack "INSERT"
    toJSON UPDATE = Data.Aeson.String $ Data.Text.pack "UPDATE"
    toJSON DELETE = Data.Aeson.String $ Data.Text.pack "DELETE"

instance Data.Aeson.FromJSON AuditAction where
    parseJSON = Data.Aeson.withText "AuditAction" $ \v ->
        case v of
            "INSERT" -> pure INSERT
            "UPDATE" -> pure UPDATE
            "DELETE" -> pure DELETE
            _ -> fail $ "Unknown value for AuditAction: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe AuditAction where
    serializeElement INSERT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "INSERT"
    serializeElement UPDATE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "UPDATE"
    serializeElement DELETE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "DELETE"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "INSERT" -> Right INSERT
        "UPDATE" -> Right UPDATE
        "DELETE" -> Right DELETE
        e -> Left ("Failed to de-serialize AuditAction, encountered unknown variant: " ++ (show bs))
    



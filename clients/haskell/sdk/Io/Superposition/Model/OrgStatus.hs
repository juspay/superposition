module Io.Superposition.Model.OrgStatus (
    OrgStatus(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for OrgStatus
data OrgStatus =
    ACTIVE
    | INACTIVE
    | PENDING_KYB
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON OrgStatus where
    toJSON ACTIVE = Data.Aeson.String $ Data.Text.pack "Active"
    toJSON INACTIVE = Data.Aeson.String $ Data.Text.pack "Inactive"
    toJSON PENDING_KYB = Data.Aeson.String $ Data.Text.pack "PendingKyb"

instance Data.Aeson.FromJSON OrgStatus where
    parseJSON = Data.Aeson.withText "OrgStatus" $ \v ->
        case v of
            "Active" -> pure ACTIVE
            "Inactive" -> pure INACTIVE
            "PendingKyb" -> pure PENDING_KYB
            _ -> fail $ "Unknown value for OrgStatus: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe OrgStatus where
    serializeElement ACTIVE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "Active"
    serializeElement INACTIVE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "Inactive"
    serializeElement PENDING_KYB = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "PendingKyb"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "Active" -> Right ACTIVE
        "Inactive" -> Right INACTIVE
        "PendingKyb" -> Right PENDING_KYB
        e -> Left ("Failed to de-serialize OrgStatus, encountered unknown variant: " ++ (show bs))
    



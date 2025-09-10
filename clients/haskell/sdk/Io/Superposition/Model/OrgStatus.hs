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
    Active
    | Inactive
    | PendingKyb
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON OrgStatus where
    toJSON Active = Data.Aeson.String $ Data.Text.pack "Active"
    toJSON Inactive = Data.Aeson.String $ Data.Text.pack "Inactive"
    toJSON PendingKyb = Data.Aeson.String $ Data.Text.pack "PendingKyb"

instance Data.Aeson.FromJSON OrgStatus where
    parseJSON = Data.Aeson.withText "OrgStatus" $ \v ->
        case v of
            "Active" -> pure Active
            "Inactive" -> pure Inactive
            "PendingKyb" -> pure PendingKyb
            _ -> fail $ "Unknown value for OrgStatus: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe OrgStatus where
    serializeElement Active = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "Active"
    serializeElement Inactive = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "Inactive"
    serializeElement PendingKyb = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "PendingKyb"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "Active" -> Right Active
        "Inactive" -> Right Inactive
        "PendingKyb" -> Right PendingKyb
        e -> Left ("Failed to de-serialize OrgStatus, encountered unknown variant: " ++ (show bs))
    



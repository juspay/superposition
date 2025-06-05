module Io.Superposition.Model.OrgStatus (
    OrgStatus(..)
) where
import qualified Data.Aeson
import qualified Data.Either
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

instance Io.Superposition.Utility.RequestSegment OrgStatus where
    toRequestSegment Active = "Active"
    toRequestSegment Inactive = "Inactive"
    toRequestSegment PendingKyb = "PendingKyb"

instance Data.Aeson.FromJSON OrgStatus where
    parseJSON = Data.Aeson.withText "OrgStatus" $ \v ->
        case v of
            "Active" -> pure Active
            "Inactive" -> pure Inactive
            "PendingKyb" -> pure PendingKyb
            _ -> fail $ "Unknown value for OrgStatus: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment OrgStatus where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "Active" -> Data.Either.Right Active
        Data.Either.Right "Inactive" -> Data.Either.Right Inactive
        Data.Either.Right "PendingKyb" -> Data.Either.Right PendingKyb
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



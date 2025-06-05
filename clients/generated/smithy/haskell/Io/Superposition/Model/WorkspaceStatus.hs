module Io.Superposition.Model.WorkspaceStatus (
    WorkspaceStatus(..)
) where
import qualified Data.Aeson
import qualified Data.Either
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

instance Io.Superposition.Utility.RequestSegment WorkspaceStatus where
    toRequestSegment ENABLED = "ENABLED"
    toRequestSegment DISABLED = "DISABLED"

instance Data.Aeson.FromJSON WorkspaceStatus where
    parseJSON = Data.Aeson.withText "WorkspaceStatus" $ \v ->
        case v of
            "ENABLED" -> pure ENABLED
            "DISABLED" -> pure DISABLED
            _ -> fail $ "Unknown value for WorkspaceStatus: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment WorkspaceStatus where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "ENABLED" -> Data.Either.Right ENABLED
        Data.Either.Right "DISABLED" -> Data.Either.Right DISABLED
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



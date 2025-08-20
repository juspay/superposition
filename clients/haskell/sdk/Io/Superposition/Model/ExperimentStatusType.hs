module Io.Superposition.Model.ExperimentStatusType (
    ExperimentStatusType(..)
) where
import qualified Data.Aeson
import qualified Data.Either
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

instance Io.Superposition.Utility.RequestSegment ExperimentStatusType where
    toRequestSegment CREATED = "CREATED"
    toRequestSegment CONCLUDED = "CONCLUDED"
    toRequestSegment INPROGRESS = "INPROGRESS"
    toRequestSegment DISCARDED = "DISCARDED"
    toRequestSegment PAUSED = "PAUSED"

instance Data.Aeson.FromJSON ExperimentStatusType where
    parseJSON = Data.Aeson.withText "ExperimentStatusType" $ \v ->
        case v of
            "CREATED" -> pure CREATED
            "CONCLUDED" -> pure CONCLUDED
            "INPROGRESS" -> pure INPROGRESS
            "DISCARDED" -> pure DISCARDED
            "PAUSED" -> pure PAUSED
            _ -> fail $ "Unknown value for ExperimentStatusType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment ExperimentStatusType where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "CREATED" -> Data.Either.Right CREATED
        Data.Either.Right "CONCLUDED" -> Data.Either.Right CONCLUDED
        Data.Either.Right "INPROGRESS" -> Data.Either.Right INPROGRESS
        Data.Either.Right "DISCARDED" -> Data.Either.Right DISCARDED
        Data.Either.Right "PAUSED" -> Data.Either.Right PAUSED
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



module Io.Superposition.Model.DimensionType (
    DimensionType(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for DimensionType
data DimensionType =
    Regular
    | LocalCohort
    | RemoteCohort
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON DimensionType where
    toJSON Regular = Data.Aeson.String $ Data.Text.pack "REGULAR"
    toJSON LocalCohort = Data.Aeson.String $ Data.Text.pack "LOCALCOHORT"
    toJSON RemoteCohort = Data.Aeson.String $ Data.Text.pack "REMOTECOHORT"

instance Data.Aeson.FromJSON DimensionType where
    parseJSON = Data.Aeson.withText "DimensionType" $ \v ->
        case v of
            "REGULAR" -> pure Regular
            "LOCALCOHORT" -> pure LocalCohort
            "REMOTECOHORT" -> pure RemoteCohort
            _ -> fail $ "Unknown value for DimensionType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe DimensionType where
    serializeElement Regular = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "REGULAR"
    serializeElement LocalCohort = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "LOCALCOHORT"
    serializeElement RemoteCohort = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "REMOTECOHORT"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "REGULAR" -> Right Regular
        "LOCALCOHORT" -> Right LocalCohort
        "REMOTECOHORT" -> Right RemoteCohort
        e -> Left ("Failed to de-serialize DimensionType, encountered unknown variant: " ++ (show bs))
    



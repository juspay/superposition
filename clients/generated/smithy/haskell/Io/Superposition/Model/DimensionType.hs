module Io.Superposition.Model.DimensionType (
    DimensionType(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for DimensionType
data DimensionType =
    Regular
    | Cohort
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON DimensionType where
    toJSON Regular = Data.Aeson.String $ Data.Text.pack "REGULAR"
    toJSON Cohort = Data.Aeson.String $ Data.Text.pack "COHORT"

instance Io.Superposition.Utility.RequestSegment DimensionType where
    toRequestSegment Regular = "REGULAR"
    toRequestSegment Cohort = "COHORT"

instance Data.Aeson.FromJSON DimensionType where
    parseJSON = Data.Aeson.withText "DimensionType" $ \v ->
        case v of
            "REGULAR" -> pure Regular
            "COHORT" -> pure Cohort
            _ -> fail $ "Unknown value for DimensionType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment DimensionType where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "REGULAR" -> Data.Either.Right Regular
        Data.Either.Right "COHORT" -> Data.Either.Right Cohort
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



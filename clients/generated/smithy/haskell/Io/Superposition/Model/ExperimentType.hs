module Io.Superposition.Model.ExperimentType (
    ExperimentType(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for ExperimentType
data ExperimentType =
    DEFAULT'
    | DELETE_OVERRIDES
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ExperimentType where
    toJSON DEFAULT' = Data.Aeson.String $ Data.Text.pack "DEFAULT"
    toJSON DELETE_OVERRIDES = Data.Aeson.String $ Data.Text.pack "DELETE_OVERRIDES"

instance Io.Superposition.Utility.RequestSegment ExperimentType where
    toRequestSegment DEFAULT' = "DEFAULT"
    toRequestSegment DELETE_OVERRIDES = "DELETE_OVERRIDES"

instance Data.Aeson.FromJSON ExperimentType where
    parseJSON = Data.Aeson.withText "ExperimentType" $ \v ->
        case v of
            "DEFAULT" -> pure DEFAULT'
            "DELETE_OVERRIDES" -> pure DELETE_OVERRIDES
            _ -> fail $ "Unknown value for ExperimentType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment ExperimentType where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "DEFAULT" -> Data.Either.Right DEFAULT'
        Data.Either.Right "DELETE_OVERRIDES" -> Data.Either.Right DELETE_OVERRIDES
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



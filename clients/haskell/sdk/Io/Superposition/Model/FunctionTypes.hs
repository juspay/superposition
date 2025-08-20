module Io.Superposition.Model.FunctionTypes (
    FunctionTypes(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for FunctionTypes
data FunctionTypes =
    Validation
    | Autocomplete
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON FunctionTypes where
    toJSON Validation = Data.Aeson.String $ Data.Text.pack "VALIDATION"
    toJSON Autocomplete = Data.Aeson.String $ Data.Text.pack "AUTOCOMPLETE"

instance Io.Superposition.Utility.RequestSegment FunctionTypes where
    toRequestSegment Validation = "VALIDATION"
    toRequestSegment Autocomplete = "AUTOCOMPLETE"

instance Data.Aeson.FromJSON FunctionTypes where
    parseJSON = Data.Aeson.withText "FunctionTypes" $ \v ->
        case v of
            "VALIDATION" -> pure Validation
            "AUTOCOMPLETE" -> pure Autocomplete
            _ -> fail $ "Unknown value for FunctionTypes: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment FunctionTypes where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "VALIDATION" -> Data.Either.Right Validation
        Data.Either.Right "AUTOCOMPLETE" -> Data.Either.Right Autocomplete
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



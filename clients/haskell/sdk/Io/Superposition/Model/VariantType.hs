module Io.Superposition.Model.VariantType (
    VariantType(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for VariantType
data VariantType =
    CONTROL
    | EXPERIMENTAL
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON VariantType where
    toJSON CONTROL = Data.Aeson.String $ Data.Text.pack "CONTROL"
    toJSON EXPERIMENTAL = Data.Aeson.String $ Data.Text.pack "EXPERIMENTAL"

instance Io.Superposition.Utility.RequestSegment VariantType where
    toRequestSegment CONTROL = "CONTROL"
    toRequestSegment EXPERIMENTAL = "EXPERIMENTAL"

instance Data.Aeson.FromJSON VariantType where
    parseJSON = Data.Aeson.withText "VariantType" $ \v ->
        case v of
            "CONTROL" -> pure CONTROL
            "EXPERIMENTAL" -> pure EXPERIMENTAL
            _ -> fail $ "Unknown value for VariantType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment VariantType where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "CONTROL" -> Data.Either.Right CONTROL
        Data.Either.Right "EXPERIMENTAL" -> Data.Either.Right EXPERIMENTAL
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



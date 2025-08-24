module Io.Superposition.Model.Version (
    Version(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for Version
data Version =
    V1
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON Version where
    toJSON V1 = Data.Aeson.String $ Data.Text.pack "V1"

instance Io.Superposition.Utility.RequestSegment Version where
    toRequestSegment V1 = "V1"

instance Data.Aeson.FromJSON Version where
    parseJSON = Data.Aeson.withText "Version" $ \v ->
        case v of
            "V1" -> pure V1
            _ -> fail $ "Unknown value for Version: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment Version where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "V1" -> Data.Either.Right V1
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



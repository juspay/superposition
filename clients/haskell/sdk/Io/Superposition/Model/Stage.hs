module Io.Superposition.Model.Stage (
    Stage(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for Stage
data Stage =
    DRAFT
    | PUBLISHED
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON Stage where
    toJSON DRAFT = Data.Aeson.String $ Data.Text.pack "draft"
    toJSON PUBLISHED = Data.Aeson.String $ Data.Text.pack "published"

instance Io.Superposition.Utility.RequestSegment Stage where
    toRequestSegment DRAFT = "draft"
    toRequestSegment PUBLISHED = "published"

instance Data.Aeson.FromJSON Stage where
    parseJSON = Data.Aeson.withText "Stage" $ \v ->
        case v of
            "draft" -> pure DRAFT
            "published" -> pure PUBLISHED
            _ -> fail $ "Unknown value for Stage: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment Stage where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "draft" -> Data.Either.Right DRAFT
        Data.Either.Right "published" -> Data.Either.Right PUBLISHED
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



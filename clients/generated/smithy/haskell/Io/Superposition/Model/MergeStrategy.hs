module Io.Superposition.Model.MergeStrategy (
    MergeStrategy(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for MergeStrategy
data MergeStrategy =
    MERGE
    | REPLACE
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON MergeStrategy where
    toJSON MERGE = Data.Aeson.String $ Data.Text.pack "MERGE"
    toJSON REPLACE = Data.Aeson.String $ Data.Text.pack "REPLACE"

instance Io.Superposition.Utility.RequestSegment MergeStrategy where
    toRequestSegment MERGE = "MERGE"
    toRequestSegment REPLACE = "REPLACE"

instance Data.Aeson.FromJSON MergeStrategy where
    parseJSON = Data.Aeson.withText "MergeStrategy" $ \v ->
        case v of
            "MERGE" -> pure MERGE
            "REPLACE" -> pure REPLACE
            _ -> fail $ "Unknown value for MergeStrategy: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment MergeStrategy where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "MERGE" -> Data.Either.Right MERGE
        Data.Either.Right "REPLACE" -> Data.Either.Right REPLACE
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



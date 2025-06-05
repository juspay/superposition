module Io.Superposition.Model.SortBy (
    SortBy(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for SortBy
data SortBy =
    Desc
    | Asc
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON SortBy where
    toJSON Desc = Data.Aeson.String $ Data.Text.pack "desc"
    toJSON Asc = Data.Aeson.String $ Data.Text.pack "asc"

instance Io.Superposition.Utility.RequestSegment SortBy where
    toRequestSegment Desc = "desc"
    toRequestSegment Asc = "asc"

instance Data.Aeson.FromJSON SortBy where
    parseJSON = Data.Aeson.withText "SortBy" $ \v ->
        case v of
            "desc" -> pure Desc
            "asc" -> pure Asc
            _ -> fail $ "Unknown value for SortBy: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment SortBy where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "desc" -> Data.Either.Right Desc
        Data.Either.Right "asc" -> Data.Either.Right Asc
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



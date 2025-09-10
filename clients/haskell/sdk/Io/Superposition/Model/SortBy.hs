module Io.Superposition.Model.SortBy (
    SortBy(..)
) where
import qualified Data.Aeson
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

instance Data.Aeson.FromJSON SortBy where
    parseJSON = Data.Aeson.withText "SortBy" $ \v ->
        case v of
            "desc" -> pure Desc
            "asc" -> pure Asc
            _ -> fail $ "Unknown value for SortBy: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe SortBy where
    serializeElement Desc = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "desc"
    serializeElement Asc = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "asc"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "desc" -> Right Desc
        "asc" -> Right Asc
        e -> Left ("Failed to de-serialize SortBy, encountered unknown variant: " ++ (show bs))
    



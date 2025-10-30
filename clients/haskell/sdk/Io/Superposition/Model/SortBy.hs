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
    DESC
    | ASC
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON SortBy where
    toJSON DESC = Data.Aeson.String $ Data.Text.pack "desc"
    toJSON ASC = Data.Aeson.String $ Data.Text.pack "asc"

instance Data.Aeson.FromJSON SortBy where
    parseJSON = Data.Aeson.withText "SortBy" $ \v ->
        case v of
            "desc" -> pure DESC
            "asc" -> pure ASC
            _ -> fail $ "Unknown value for SortBy: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe SortBy where
    serializeElement DESC = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "desc"
    serializeElement ASC = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "asc"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "desc" -> Right DESC
        "asc" -> Right ASC
        e -> Left ("Failed to de-serialize SortBy, encountered unknown variant: " ++ (show bs))
    



module Io.Superposition.Model.ImportOnError (
    ImportOnError(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for ImportOnError
data ImportOnError =
    ABORT
    | CONTINUE
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON ImportOnError where
    toJSON ABORT = Data.Aeson.String $ Data.Text.pack "abort"
    toJSON CONTINUE = Data.Aeson.String $ Data.Text.pack "continue"

instance Data.Aeson.FromJSON ImportOnError where
    parseJSON = Data.Aeson.withText "ImportOnError" $ \v ->
        case v of
            "abort" -> pure ABORT
            "continue" -> pure CONTINUE
            _ -> fail $ "Unknown value for ImportOnError: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe ImportOnError where
    serializeElement ABORT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "abort"
    serializeElement CONTINUE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "continue"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "abort" -> Right ABORT
        "continue" -> Right CONTINUE
        e -> Left ("Failed to de-serialize ImportOnError, encountered unknown variant: " ++ (show bs))
    



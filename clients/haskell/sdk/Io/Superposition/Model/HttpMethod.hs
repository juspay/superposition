module Io.Superposition.Model.HttpMethod (
    HttpMethod(..)
) where
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for HttpMethod
data HttpMethod =
    GET
    | POST
    | PUT
    | PATCH
    | DELETE
    | HEAD
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON HttpMethod where
    toJSON GET = Data.Aeson.String $ Data.Text.pack "GET"
    toJSON POST = Data.Aeson.String $ Data.Text.pack "POST"
    toJSON PUT = Data.Aeson.String $ Data.Text.pack "PUT"
    toJSON PATCH = Data.Aeson.String $ Data.Text.pack "PATCH"
    toJSON DELETE = Data.Aeson.String $ Data.Text.pack "DELETE"
    toJSON HEAD = Data.Aeson.String $ Data.Text.pack "HEAD"

instance Data.Aeson.FromJSON HttpMethod where
    parseJSON = Data.Aeson.withText "HttpMethod" $ \v ->
        case v of
            "GET" -> pure GET
            "POST" -> pure POST
            "PUT" -> pure PUT
            "PATCH" -> pure PATCH
            "DELETE" -> pure DELETE
            "HEAD" -> pure HEAD
            _ -> fail $ "Unknown value for HttpMethod: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe HttpMethod where
    serializeElement GET = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "GET"
    serializeElement POST = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "POST"
    serializeElement PUT = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "PUT"
    serializeElement PATCH = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "PATCH"
    serializeElement DELETE = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "DELETE"
    serializeElement HEAD = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "HEAD"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "GET" -> Right GET
        "POST" -> Right POST
        "PUT" -> Right PUT
        "PATCH" -> Right PATCH
        "DELETE" -> Right DELETE
        "HEAD" -> Right HEAD
        e -> Left ("Failed to de-serialize HttpMethod, encountered unknown variant: " ++ (show bs))
    



module Io.Superposition.Model.HttpMethod (
    HttpMethod(..)
) where
import qualified Data.Aeson
import qualified Data.Either
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

instance Io.Superposition.Utility.RequestSegment HttpMethod where
    toRequestSegment GET = "GET"
    toRequestSegment POST = "POST"
    toRequestSegment PUT = "PUT"
    toRequestSegment PATCH = "PATCH"
    toRequestSegment DELETE = "DELETE"
    toRequestSegment HEAD = "HEAD"

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
        
    

instance Io.Superposition.Utility.ResponseSegment HttpMethod where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "GET" -> Data.Either.Right GET
        Data.Either.Right "POST" -> Data.Either.Right POST
        Data.Either.Right "PUT" -> Data.Either.Right PUT
        Data.Either.Right "PATCH" -> Data.Either.Right PATCH
        Data.Either.Right "DELETE" -> Data.Either.Right DELETE
        Data.Either.Right "HEAD" -> Data.Either.Right HEAD
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    



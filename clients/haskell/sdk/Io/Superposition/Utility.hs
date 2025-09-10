module Io.Superposition.Utility (
    SerDe (..),
    setMethod,
    setPath,
    serHeader,
    serHeaderMap,
    serQuery,
    serQueryMap,
    SerializeBody (..),
    serField,
    FromResponseParser (..),
    deSerHeader,
    deSerHeaderMap,
    deSerField,
    DeSerializeBody (..),
    IntoRequestBuilder (..),
    OperationError (..),
    RawRequest (..),
    RawResponse (..),
    HttpMetadata (..),
    runOperation
) where


import Control.Monad ((>=>))
import qualified Control.Monad.State.Strict as MTL
import Data.Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Bifunctor
import qualified Data.Bifunctor as Bifunctor
import Data.ByteString (ByteString, StrictByteString, toStrict)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as Char8 (unpack)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Foldable (traverse_)
import Data.Function
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map as M
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text, pack, toLower, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.Word as BSW
import GHC.Generics (Generic)
import GHC.Int (Int32)
import Network.HTTP.Client (BodyReader, Response)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Date (HTTPDate, formatHTTPDate, parseHTTPDate)
import qualified Network.HTTP.Types as HTTP
import qualified Network.URI as Network
import System.Environment (lookupEnv)
import Text.Read (readEither)

instance ToJSON HTTPDate where
  toJSON = Data.Aeson.String . decodeUtf8 . formatHTTPDate

instance FromJSON HTTPDate where
  parseJSON = withText "HTTPDate" $ \t ->
    parseHTTPDate (encodeUtf8 t)
      & maybe (fail "Failed to parse HTTP date") pure

-- | Type class for things used in headers, query-params, url params or even to
-- serialize the payload.
class SerDe t where
  serializeElement :: t -> StrictByteString
  deSerializeElement :: StrictByteString -> Either String t

  default serializeElement :: (ToJSON t) => t -> StrictByteString
  serializeElement = toStrict . encode
  default deSerializeElement :: (FromJSON t) => StrictByteString -> Either String t
  deSerializeElement = eitherDecodeStrict

-- | Smithy Byte
instance SerDe Int8

-- | Smithy Short
instance SerDe Int16

-- | Smithy Integer
instance SerDe Int32

-- | Smithy Long
instance SerDe Int64

-- | Smithy Float
instance SerDe Float

-- | Smithy Double
instance SerDe Double

-- | Smithy Boolean
instance SerDe Bool

-- | Smithy BigInteger
instance SerDe Integer

-- | Smithy Document
instance SerDe Aeson.Value

-- | Smithy Blob
instance SerDe StrictByteString where
  serializeElement = id
  deSerializeElement = Right

-- | All string & string-like types require special handling.
-- We need to ensure that quotes are trimmed when serializing and added when
-- attempting to deserialize, as per smithy's contracts:
-- Query: https://smithy.io/2.0/spec/http-bindings.html#id12
-- Path: https://smithy.io/2.0/spec/http-bindings.html#id8
-- Header: https://smithy.io/2.0/spec/http-bindings.html#serialization-rules
instance SerDe Text where
  serializeElement = encodeUtf8
  deSerializeElement = Right . decodeUtf8

encodeTrimQuotes :: (ToJSON t) => t -> StrictByteString
encodeTrimQuotes = BS.init . BS.tail . toStrict . encode

quoteAndDecode :: (FromJSON t) => StrictByteString -> Either String t
quoteAndDecode bs = eitherDecodeStrict ("\"" <> bs <> "\"")

-- Smithy date-time
instance SerDe UTCTime where
  serializeElement = encodeTrimQuotes
  deSerializeElement = quoteAndDecode

-- Smithy http-date
instance SerDe HTTPDate where
  serializeElement = encodeTrimQuotes
  deSerializeElement = quoteAndDecode

-- Smithy Timestamp
-- Serializes to a json-number, so this is not a `string-like` type.
instance SerDe POSIXTime

type Path = [StrictByteString]

type ContentType = String

data RequestBody
  = NoBody
  | Opaque ContentType StrictByteString
  | Json Aeson.Object

getBodyContent :: RequestBody -> LazyByteString
getBodyContent NoBody = ""
getBodyContent (Opaque _ bs) = LBS.fromStrict bs
getBodyContent (Json obj) = encode obj

data RequestBuilderSt = RequestBuilderSt
  { _headers :: M.Map HTTP.HeaderName [StrictByteString],
    _method :: HTTP.Method,
    _body :: RequestBody,
    _query :: M.Map StrictByteString [StrictByteString],
    _path :: Path
  }

type RequestBuilder = MTL.State RequestBuilderSt

newRequestBuilderSt :: RequestBuilderSt
newRequestBuilderSt =
  RequestBuilderSt
    { _headers = mempty,
      _method = HTTP.methodGet,
      _body = NoBody,
      _query = mempty,
      _path = mempty
    }

-- Trim a specific byte from both ends
trimByte :: BSW.Word8 -> BS.ByteString -> BS.ByteString
trimByte byte bs = BS.dropWhile (== byte) (BS.dropWhileEnd (== byte) bs)

mergeWithHTTPRequest :: RequestBuilderSt -> HTTP.Request -> HTTP.Request
mergeWithHTTPRequest st req =
  req
    { HTTP.method = _method st,
      -- Headers can be added multiple times or stored as a comma-separated list.
      -- See: https://smithy.io/2.0/spec/http-bindings.html#serialization-rules
      HTTP.requestHeaders = M.toList $ M.map (BS.intercalate ",") (_headers st),
      -- Query params which store a list need to be added multiple times.
      -- See: https://smithy.io/2.0/spec/http-bindings.html#id12
      HTTP.queryString = HTTP.renderSimpleQuery False $ M.foldMapWithKey (\k v -> map (k,) v) (_query st),
      -- 47 is `/`, trimming it to avoid `//` in paths.
      HTTP.path = trimByte 47 (HTTP.path req) <> "/" <> BS.intercalate "/" (map (HTTP.urlEncode False) (_path st)),
      HTTP.requestBody = case _body st of
        NoBody -> HTTP.requestBody req
        Opaque ct bs -> HTTP.RequestBodyLBS (BS.fromStrict bs)
        Json obj -> HTTP.RequestBodyLBS (encode obj)
    }

data ParamLocation = Query | Header

class SerializeParameter p where
  serParameter :: ParamLocation -> String -> p -> RequestBuilder ()

instance {-# OVERLAPPABLE #-} (SerDe p) => SerializeParameter p where
  serParameter location name value =
    let v = serializeElement value
        f = case location of
          Query -> \s -> s {_query = M.insert (fromString name) [v] (_query s)}
          Header -> \s -> s {_headers = M.insert (fromString name) [v] (_headers s)}
     in MTL.modify f

instance (SerDe p, SerializeParameter p) => SerializeParameter [p] where
  serParameter location name value =
    let v = map serializeElement value
        f = case location of
          Query -> \s -> s {_query = M.insert (fromString name) v (_query s)}
          Header -> \s -> s {_headers = M.insert (fromString name) v (_headers s)}
     in MTL.modify f

instance (SerializeParameter p) => SerializeParameter (Maybe p) where
  -- Only add it if it's non-null.
  serParameter location name = traverse_ (serParameter location name)

data ParameterMapT
  = QueryMap
  | HeaderMap String -- Prefix

class SerializeParameterMap p where
  serParameterMap :: ParameterMapT -> p -> RequestBuilder ()

instance (SerializeParameter p) => SerializeParameterMap (M.Map Text p) where
  serParameterMap QueryMap = traverse_ (\(k, v) -> serQuery (T.unpack k) v) . M.toList
  serParameterMap (HeaderMap prefix) =
    traverse_ (\(k, v) -> serParameter Header (T.unpack $ fromString prefix <> k) v) . M.toList

instance (SerializeParameter p) => SerializeParameterMap (Maybe (M.Map Text p)) where
  serParameterMap pm = traverse_ (serParameterMap pm)

serHeader :: (SerializeParameter t) => String -> t -> RequestBuilder ()
serHeader = serParameter Header

serHeaderMap :: (SerializeParameterMap t) => String -> t -> RequestBuilder ()
serHeaderMap = serParameterMap . HeaderMap

serQuery :: (SerializeParameter t) => String -> t -> RequestBuilder ()
serQuery = serParameter Query

serQueryMap :: (SerializeParameterMap t) => t -> RequestBuilder ()
serQueryMap = serParameterMap QueryMap

setMethod :: HTTP.Method -> RequestBuilder ()
setMethod method = MTL.modify (\s -> s {_method = method})

setContentType :: RequestBuilder ()
setContentType = do
  body <- MTL.gets _body
  case body of
    NoBody -> pure ()
    Opaque ct _ -> serHeader "Content-Type" (fromString ct :: Text)
    Json _ -> serHeader "Content-Type" ("application/json" :: Text)

class SerializeBody t where
  serBody :: String -> t -> RequestBuilder ()

  -- | For `structures` we can default to JSON serialization.
  default serBody :: (ToJSON t) => String -> t -> RequestBuilder ()
  serBody contentType body =
    MTL.modify (\s -> s {_body = Opaque contentType (toStrict $ encode body)})

-- | Setting the body is curious, it's possible that the model binds the body
-- - to a string or some other type that is not a `structure`, and has different
-- - serialization rules: https://smithy.io/2.0/spec/http-bindings.html#id10
-- - This is why using `serializeElement` is the right choice here, it is compliant
-- - w/ this contract.
instance {-# OVERLAPPABLE #-} (SerDe t) => SerializeBody t where
  -- `Content-Type` is required to be set by the caller due to the `mediaType` trait.
  serBody contentType body =
    MTL.modify (\s -> s {_body = Opaque contentType (serializeElement body)})

instance (SerializeBody t) => SerializeBody (Maybe t) where
  serBody contentType = traverse_ (serBody contentType)

-- | Serializing fields should direclty use `Aeson`.
serField :: (ToJSON t) => Aeson.Key -> t -> RequestBuilder ()
serField k v = do
  body <- MTL.gets _body
  let obj = case body of
        Json obj -> obj
        _ -> mempty
  MTL.modify (\s -> s {_body = Json $ Aeson.insert k (Aeson.toJSON v) obj})

setPath :: Path -> RequestBuilder ()
setPath path = MTL.modify (\s -> s {_path = path})

type HttpResponse = Response BodyReader

-- | Lazly parsed body, to avoid re-parsing into an object if we need
-- - to parse out multiple fields from it.
data Body = Raw StrictByteString | Obj Aeson.Object

-- | Prases a type from an HTTP response.
newtype HttpResponseParser a
  = HttpResponseParser
  { runParser :: (HttpResponse, Body) -> (Either String a, Body)
  }

instance Functor HttpResponseParser where
  fmap f (HttpResponseParser p) = HttpResponseParser $ \r ->
    let (ea, b) = p r in (fmap f ea, b)

instance Applicative HttpResponseParser where
  pure a = HttpResponseParser $ \r -> (Right a, snd r)
  HttpResponseParser pf <*> HttpResponseParser pa = HttpResponseParser $ \r ->
    let (ef, b1) = pf r
        (ea, b2) = pa (fst r, b1)
     in (ef <*> ea, b2)

instance Monad HttpResponseParser where
  HttpResponseParser p >>= f = HttpResponseParser $ \r ->
    let (ea, b1) = p r
     in case ea of
          Right a -> runParser (f a) (fst r, b1)
          Left e -> (Left e, b1)

class DeSerializeHeader t where
  deSerHeader :: String -> HttpResponseParser t

instance {-# OVERLAPPABLE #-} (SerDe t) => DeSerializeHeader t where
  deSerHeader name = HttpResponseParser $ \(r, b) ->
    let headers = HTTP.responseHeaders r
        mHeader = lookup (fromString name) headers
     in case mHeader of
          Just v -> (deSerializeElement v, b)
          Nothing -> (Left $ "Header not found: " ++ name, b)

instance (SerDe t) => DeSerializeHeader [t] where
  deSerHeader name = HttpResponseParser $ \(r, b) ->
    let (bs :: Either String StrictByteString, _) = runParser (deSerHeader name) (r, b)
     in case bs of
          -- Split on commas (44 is the ASCII code for comma) & then de-serialize each value.
          Right v -> (traverse deSerializeElement (BS.split 44 v), b)
          Left e -> (Left e, b)

instance (DeSerializeHeader t) => DeSerializeHeader (Maybe t) where
  deSerHeader name = HttpResponseParser $ \(r, b) ->
    case runParser (deSerHeader name) (r, b) of
      (Right v, _) -> (Right (Just v), b)
      _ -> (Right Nothing, b)

class DeSerializeHeaderMap t where
  deSerHeaderMap :: String -> HttpResponseParser t

instance (DeSerializeHeader t) => DeSerializeHeaderMap (M.Map Text t) where
  deSerHeaderMap prefix = do
    headers <- HTTP.responseHeaders <$> getResponse
    let prefix' = T.toLower $ fromString prefix
        -- Unfortunately, CI doesn't have `prefixOf` function, so will have to
        -- convert to Text.
        relevantHeaders =
          filter (T.isPrefixOf prefix') $
            map (T.toLower . decodeUtf8 . CI.original . fst) headers
    -- Strip prefix before returning to user.
    M.fromList <$> traverse (\h -> (fromMaybe h $ T.stripPrefix prefix' h,) <$> deSerHeader (T.unpack h)) relevantHeaders

instance (DeSerializeHeader t) => DeSerializeHeaderMap (Maybe (M.Map Text t)) where
  deSerHeaderMap prefix = HttpResponseParser $ \(r, b) ->
    case runParser (deSerHeaderMap prefix) (r, b) of
      (Right v, _) -> (Right (Just v), b)
      _ -> (Right Nothing, b)

getResponse :: HttpResponseParser HttpResponse
getResponse = HttpResponseParser $ Bifunctor.first Right

embed :: Either String a -> HttpResponseParser a
embed ea = HttpResponseParser $ \(_, b) -> (ea, b)

parseError :: String -> HttpResponseParser a
parseError msg = HttpResponseParser $ \(_, b) -> (Left msg, b)

getContentType :: HttpResponseParser ByteString
getContentType = HttpResponseParser $ \(r, b) ->
  case lookup "Content-Type" (HTTP.responseHeaders r) of
    Just ct -> (Right ct, b)
    Nothing -> (Left "No content-type available.", b)

getBody :: HttpResponseParser Body
getBody = HttpResponseParser $ \(r, b) -> (Right b, b)

-- There's probably a better way to do this, but this works for now.
-- At-least the behaviour is well defined.
class DeSerializeBody t where
  deSerBody :: HttpResponseParser t

instance {-# OVERLAPPABLE #-} (FromJSON t) => DeSerializeBody t where
  deSerBody = do
    ctype <- getContentType
    body <- getBody
    if ctype == "application/json"
      then case body of
        Raw bs -> embed $ eitherDecodeStrict bs
        Obj o -> embed $ eitherDecode $ encode o
      else parseError $ "Unsupported content-type: " ++ show ctype

instance DeSerializeBody Text where
  deSerBody = do
    ctype <- getContentType
    body <- getBody
    case ctype of
      "application/json" -> case body of
        Raw bs -> embed $ eitherDecodeStrict bs
        Obj o -> embed $ eitherDecode $ encode o
      "text/plain" -> case body of
        Raw bs -> pure $ decodeUtf8 bs
        Obj o -> pure $ decodeUtf8 $ toStrict $ encode o
      ct -> parseError $ "Unsupported content-type: " ++ show ct

instance DeSerializeBody StrictByteString where
  deSerBody = do
    ctype <- getContentType
    body <- getBody
    case ctype of
      "application/octet-stream" -> case body of
        Raw bs -> pure bs
        Obj o -> pure $ toStrict $ encode o
      ct -> parseError $ "Unsupported content-type: " ++ show ct

deSerField :: (FromJSON t) => Key -> HttpResponseParser t
deSerField key = HttpResponseParser $ \(_, body) ->
  -- NOTE `application/json` should be asserted here
  let decoded = case body of
        Raw bs -> eitherDecodeStrict bs
        Obj o -> Right o
      parse = Aeson.parseEither (Aeson..: key)
   in -- NOTE Need to change the body to it's decoded version to avoid re-parsing.
      (decoded >>= parse, either (const body) Obj decoded)

class IntoRequestBuilder t where
  intoRequestBuilder :: t -> RequestBuilder ()

class FromResponseParser t where
  expectedStatus :: HTTP.Status
  responseParser :: HttpResponseParser t

data RawRequest = RawRequest
  { requestHeaders :: [HTTP.Header],
    requestQuery :: ByteString,
    requestBody :: Maybe LazyByteString,
    requestMethod :: HTTP.Method,
    requestPath :: ByteString
  }
  deriving (Generic, Show)

instance ToJSON RawRequest where
  toJSON rawReq =
    let utf8Headers =
          map
            (\(k, v) -> (decodeUtf8 $ CI.original k, decodeUtf8 v))
            (requestHeaders rawReq)
        utf8Query = decodeUtf8 (requestQuery rawReq)
     in Aeson.object
          [ "method" Aeson..= decodeUtf8 (requestMethod rawReq),
            "path" Aeson..= decodeUtf8 (requestPath rawReq),
            "query" Aeson..= utf8Query,
            "headers" Aeson..= utf8Headers,
            "body" Aeson..= (TL.decodeUtf8 <$> requestBody rawReq)
          ]

fromRequest :: HTTP.Request -> LazyByteString -> RawRequest
fromRequest req body =
  RawRequest
    { requestHeaders = HTTP.requestHeaders req,
      requestQuery = HTTP.queryString req,
      requestBody = Just body,
      requestMethod = HTTP.method req,
      requestPath = HTTP.path req
    }

data RawResponse = RawResponse
  { responseHeaders :: [HTTP.Header],
    responseBody :: ByteString,
    responseStatus :: Int
  }
  deriving (Generic, Show)

instance ToJSON RawResponse where
  toJSON rawResp =
    let utf8Headers =
          map
            (\(k, v) -> (decodeUtf8 $ CI.original k, decodeUtf8 v))
            (responseHeaders rawResp)
     in Aeson.object
          [ "status" Aeson..= responseStatus rawResp,
            "headers" Aeson..= utf8Headers,
            "body" Aeson..= decodeUtf8 (responseBody rawResp)
          ]

fromResponse :: HttpResponse -> ByteString -> RawResponse
fromResponse resp body =
  RawResponse
    { responseHeaders = HTTP.responseHeaders resp,
      responseBody = body,
      responseStatus = HTTP.statusCode (HTTP.responseStatus resp)
    }

data HttpMetadata = HttpMetadata
  { rawRequest :: RawRequest,
    rawResponse :: RawResponse
  }
  deriving (Generic, Show)

instance ToJSON HttpMetadata

class OperationError e where
  getErrorParser :: HTTP.Status -> Maybe (HttpResponseParser e)
  mkBuilderError :: Text -> e
  mkDeSerializationError :: HttpMetadata -> Text -> e
  mkUnexpectedError :: Maybe HttpMetadata -> Text -> e

badHttpUrl :: Text
badHttpUrl = "The provided endpoint is not a well-formed http url."

runOperation ::
  (FromResponseParser t, OperationError e, IntoRequestBuilder i) =>
  Network.URI ->
  HTTP.Manager ->
  RequestBuilder () ->
  Either Text i ->
  IO (Either e t)
runOperation _ _ _ (Left err) =
  pure $
    Left $
      mkBuilderError err
runOperation endpoint manager setAuth (Right i) = do
  let rbuilder = intoRequestBuilder i >> setAuth >> setContentType
      (_, reqSt) = MTL.runState rbuilder newRequestBuilderSt
      initReq = mergeWithHTTPRequest reqSt <$> HTTP.requestFromURI endpoint
      rawBody = getBodyContent (_body reqSt)
  case initReq of
    Just req -> HTTP.withResponse req manager (parseOutput rawBody)
    -- NOTE Should we create this in the client it-self? Would make things alot simpler IMO.
    _ -> pure (Left $ mkUnexpectedError Nothing badHttpUrl)

parseOutput ::
  forall t e.
  (FromResponseParser t, OperationError e) =>
  LazyByteString ->
  HttpResponse ->
  IO (Either e t)
parseOutput rawBody response = do
  body <- HTTP.brRead (HTTP.responseBody response)
  let status = HTTP.responseStatus response
      code = HTTP.statusCode status
      parseInput = (response, Raw body)
      rawResp = fromResponse response body
      rawReq = fromRequest (HTTP.getOriginalRequest response) rawBody
      metadata = HttpMetadata rawReq rawResp
  if 299 >= code && code >= 200
    then case runParser responseParser parseInput of
      (Right v, _) -> pure $ Right v
      (Left e, _) -> pure $ Left $ mkDeSerializationError metadata (pack e)
    else case getErrorParser status of
      Just p -> pure $ case runParser p parseInput of
        (Right v, _) -> Left v
        (Left e, _) -> Left $ mkDeSerializationError metadata (pack e)
      Nothing -> pure $ Left $ mkUnexpectedError (Just metadata) "Un-expected status code."


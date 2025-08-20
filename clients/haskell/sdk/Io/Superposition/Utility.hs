module Io.Superposition.Utility (
    RequestSegment,
    toRequestSegment,
    ResponseSegment,
    fromResponseSegment,
    mapLeft
) where


import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as Char8 (unpack)
import Data.Function
import Data.Int (Int16, Int64, Int8)
import Data.Maybe
import Data.Text (Text, pack, toLower, unpack)
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Time (defaultTimeLocale, parseTimeM, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Network.HTTP.Date (HTTPDate, formatHTTPDate, parseHTTPDate)
import Text.Read (readEither)

instance ToJSON HTTPDate where
  toJSON = Data.Aeson.String . decodeUtf8 . formatHTTPDate

instance FromJSON HTTPDate where
  parseJSON = withText "HTTPDate" $ \t ->
    parseHTTPDate (encodeUtf8 t)
      & maybe (fail "Failed to parse HTTP date") pure

class RequestSegment a where
  toRequestSegment :: (Show a) => a -> Text

class ResponseSegment a where
  fromResponseSegment :: ByteString -> Either Text a

instance RequestSegment Int64 where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Int64 where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Int16 where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Int16 where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Int8 where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Int8 where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Integer where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Integer where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Double where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Double where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Float where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Float where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment Text where
  toRequestSegment = id

instance ResponseSegment Text where
  fromResponseSegment = mapLeft (pack . show) . decodeUtf8'

instance RequestSegment Bool where
  toRequestSegment = toLower . pack . show

instance ResponseSegment Bool where
  fromResponseSegment = mapLeft pack . eitherDecodeStrict'

instance RequestSegment HTTPDate where
  toRequestSegment = decodeUtf8 . formatHTTPDate

instance ResponseSegment HTTPDate where
  fromResponseSegment = maybe (Left "Failed to parse HTTPDate") Right . parseHTTPDate

instance RequestSegment UTCTime where
  toRequestSegment utc = pack $ (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" utc) ++ "+00:00"

instance ResponseSegment UTCTime where
  fromResponseSegment = mapLeft pack . readUTCTime . Char8.unpack
    where
      readUTCTime :: String -> Either String UTCTime
      readUTCTime ts = case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" ts of
        Just ts -> Right ts
        _ -> Left "Failed to parse as UTCTime."

instance RequestSegment POSIXTime where
  toRequestSegment = pack . show

instance ResponseSegment POSIXTime where
  fromResponseSegment = mapLeft pack . readEither . Char8.unpack

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right



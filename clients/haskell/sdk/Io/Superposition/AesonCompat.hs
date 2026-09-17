-- | Shim for building the SDK against aeson 1.5 (GHC 8.10 / lts-18.28), where object keys are
--   'Text' and there is no @Data.Aeson.KeyMap@. Imported under the @Aeson@ alias by "Io.Superposition.Utility".
module Io.Superposition.AesonCompat
  ( Key,
    insert,
    StrictByteString,
    LazyByteString,
  )
where

import           Data.Aeson                (Value)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.HashMap.Strict       as HM
import           Data.Text                 (Text)

-- | aeson 2 calls this @Data.Aeson.Key@; in aeson 1.5 object keys are 'Text'.
type Key = Text

-- | bytestring < 0.11.2 has no such synonyms.
type StrictByteString = BS.ByteString

type LazyByteString = LBS.ByteString

insert :: Key -> Value -> HM.HashMap Text Value -> HM.HashMap Text Value
insert = HM.insert

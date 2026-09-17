module Data.OpenFeature.SuperpositionProviderOptions where

import Control.Monad.Logger (LogLevel (..))
import Data.Aeson (ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.URI qualified as Net

data RefreshOptions = OnDemand Int64 | Poll Int
  deriving (Generic, Read, Show, ToJSON, Eq)

-- Orphans: LogLevel and URI come from libraries that do not provide these, and the options record
-- cannot derive Show/Eq/ToJSON without them.
deriving instance Generic LogLevel

deriving instance ToJSON LogLevel

deriving instance ToJSON Net.URIAuth

deriving instance ToJSON Net.URI

data SuperpositionProviderOptions = SuperpositionProviderOptions
  { orgId :: Text,
    workspaceId :: Text,
    endpoint :: Net.URI,
    token :: Text,
    -- TODO
    fallbackConfig :: (),
    refreshOptions :: RefreshOptions,
    experimentationRefreshOptions :: Maybe RefreshOptions,
    -- | Only fetch config keys under these prefixes. Nothing fetches the whole workspace config.
    configPrefixes :: Maybe [Text],
    logLevel :: LogLevel
  }
  deriving (Generic, Show, ToJSON, Eq)

defaultProviderOptions :: SuperpositionProviderOptions
defaultProviderOptions =
  SuperpositionProviderOptions
    { orgId = "",
      workspaceId = "",
      endpoint = Net.URI "" Nothing "" "" "",
      token = "",
      fallbackConfig = (),
      refreshOptions = OnDemand 0,
      experimentationRefreshOptions = Nothing,
      configPrefixes = Nothing,
      logLevel = LevelError
    }

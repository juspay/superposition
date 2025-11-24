module Data.OpenFeature.SuperpositionProviderOptions where

import Control.Monad.Logger (LogLevel (..))
import Data.Int (Int64)
import Data.Text (Text)
import Network.URI qualified as Net
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data RefreshOptions = OnDemand Int64 | Poll Int
  deriving (Generic, Read, Show, ToJSON, Eq)

data Authorization = Bearer Text | Basic Text
  deriving (Generic, Read, Show, ToJSON, Eq)

-- HACK Orphan instances :(
deriving instance Generic LogLevel
deriving instance ToJSON LogLevel
deriving instance ToJSON Net.URIAuth
deriving instance ToJSON Net.URI

data SuperpositionProviderOptions = SuperpositionProviderOptions
  { orgId :: Text,
    workspaceId :: Text,
    endpoint :: Net.URI,
    auth :: Authorization,
    -- TODO
    -- fallbackConfig :: (),
    refreshOptions :: RefreshOptions,
    experimentationRefreshOptions :: Maybe RefreshOptions,
    logLevel :: LogLevel
  }
  deriving (Generic, Show, ToJSON, Eq)

defaultProviderOptions :: SuperpositionProviderOptions
defaultProviderOptions =
  SuperpositionProviderOptions
    { orgId = "",
      workspaceId = "",
      endpoint = Net.URI "" Nothing "" "" "",
      auth = Bearer "",
      -- fallbackConfig = (),
      refreshOptions = OnDemand 0,
      experimentationRefreshOptions = Nothing,
      logLevel = LevelError
    }

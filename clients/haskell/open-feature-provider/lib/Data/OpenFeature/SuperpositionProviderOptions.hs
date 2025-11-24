module Data.OpenFeature.SuperpositionProviderOptions where

import Control.Monad.Logger (LogLevel (..))
import Data.Int (Int64)
import Data.Text (Text)
import Network.URI qualified as Net

data RefreshOptions = OnDemand Int64 | Poll Int

data Authorization = Bearer Text | Basic Text

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

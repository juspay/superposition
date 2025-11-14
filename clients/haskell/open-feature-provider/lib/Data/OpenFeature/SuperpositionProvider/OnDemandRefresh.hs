module Data.OpenFeature.SuperpositionProvider.OnDemandRefresh (OnDemandRefresh (..)) where

import Control.Applicative ((<|>))
import Data.Int (Int64)
import Data.OpenFeature.SuperpositionProvider.RefreshTask
import Data.Time.Clock.System qualified as Time
import GHC.Conc.Sync

type Timestamp = Int64

data OnDemandRefresh a = OnDemandRefresh
  { rFn :: RefreshFn a,
    ttl :: Int64,
    cache :: TVar (Maybe (a, Timestamp))
  }

refreshValue :: OnDemandRefresh a -> IO (Maybe a)
refreshValue r = do
  v <- rFn r
  tsNew <- Time.systemSeconds <$> Time.getSystemTime
  mapM_ (atomically . writeTVar (cache r) . Just . (,tsNew)) v
  pure v

instance RefreshTask OnDemandRefresh a where
  isRunning _ = pure True
  startRefresh _ = pure ()
  stopRefresh _ = pure ()
  getCurrent r = do
    curr <- readTVarIO (cache r)
    now <- Time.systemSeconds <$> Time.getSystemTime
    get curr now
    where
      get (Just (v, ts)) now
        -- past deadline, try to refresh
        | ts + ttl r < now = do
            new <- refreshValue r
            -- return previous value if refresh failed
            pure $ new <|> Just v
        | otherwise = pure $ Just v
      get _ _ = refreshValue r

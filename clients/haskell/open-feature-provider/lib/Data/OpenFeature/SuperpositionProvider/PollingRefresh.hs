module Data.OpenFeature.SuperpositionProvider.PollingRefresh (PollingRefresh (..)) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)
import Data.OpenFeature.SuperpositionProvider.RefreshTask
import GHC.Base (when)
import GHC.Conc.Sync as Sync
import GHC.MVar as MVar

data PollingRefresh a = PollingRefresh
  { interval :: Int,
    rFn :: RefreshFn a,
    chan :: TVar (Maybe a),
    tid :: MVar ThreadId
  }

instance RefreshTask PollingRefresh a where
  isRunning r = not <$> (isEmptyMVar (tid r))
  startRefresh r = do
    st <- isRunning r
    unless st $ do
      t <- forkIO poll
      putMVar (tid r) t
    where
      poll =
        forever $
          rFn r
            >>= mapM_ (atomically . writeTVar (chan r) . Just)
            >> threadDelay ((interval r) * 1000000)
  stopRefresh r = isRunning r >>= \b -> when b (takeMVar (tid r) >>= killThread)
  getCurrent r = readTVarIO (chan r)

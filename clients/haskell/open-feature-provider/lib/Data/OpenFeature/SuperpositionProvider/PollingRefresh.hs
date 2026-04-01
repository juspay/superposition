module Data.OpenFeature.SuperpositionProvider.PollingRefresh (PollingRefresh (..)) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void)
import Data.IORef (IORef)
import Data.OpenFeature.SuperpositionProvider.RefreshTask
import GHC.Conc.Sync as Sync
import System.Mem.Weak (mkWeak, deRefWeak)

data PollingRefresh a = PollingRefresh
  { interval :: Int,
    rFn :: RefreshFn a,
    chan :: TVar (Maybe a),
    anchor :: IORef (),
    onRefresh :: Maybe (IO ())
  }

instance RefreshTask PollingRefresh a where
  isRunning _ = pure True
  startRefresh r = do
    weak <- mkWeak (anchor r) () Nothing
    let poll = do
          alive <- deRefWeak weak
          case alive of
            Nothing -> pure ()
            Just _ -> do
              rFn r >>= mapM_ (\v -> do
                atomically $ writeTVar (chan r) (Just v)
                maybe (pure ()) id (onRefresh r))
              threadDelay (interval r * 1000000)
              poll
    void $ forkIO poll
  stopRefresh _ = pure ()
  getCurrent r = readTVarIO (chan r)

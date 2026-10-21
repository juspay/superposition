module Data.OpenFeature.SuperpositionProvider.PollingRefresh (PollingRefresh (..)) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (evaluate)
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
    -- Force the record selector before using the IORef as a weak key.
    -- 'mkWeak' does not evaluate its key, so keying on the unevaluated thunk
    -- 'anchor r' makes the weak pointer die as soon as the thunk is GC'd,
    -- even though the IORef itself is still held by the provider.
    a <- evaluate (anchor r)
    weak <- mkWeak a () Nothing
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

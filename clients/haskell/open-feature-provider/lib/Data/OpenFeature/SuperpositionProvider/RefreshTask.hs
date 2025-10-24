module Data.OpenFeature.SuperpositionProvider.RefreshTask where

import GHC.Base (Type)

type RefreshFn a = IO (Maybe a)

class RefreshTask (r :: Type -> Type) a where
  isRunning :: r a -> IO Bool
  startRefresh :: r a -> IO ()
  stopRefresh :: r a -> IO ()
  getCurrent :: r a -> IO (Maybe a)

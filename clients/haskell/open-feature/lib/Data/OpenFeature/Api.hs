{-# LANGUAGE ExistentialQuantification #-}

module Data.OpenFeature.Api
  ( OpenFeature (..),
    DynFeatureProvider (..),
    openFeatureTVar,
    setNamedProvider,
    setDefaultProvider,
    setGlobalContext,
  )
where

import Data.Map qualified as Map
import Data.OpenFeature.EvaluationContext
import Data.OpenFeature.FeatureProvider
import Data.Text (Text)
import GHC.Conc qualified as Conc
import System.IO.Unsafe (unsafePerformIO)

data DynFeatureProvider = forall p. (FeatureProvider p) => DynFeatureProvider p

data OpenFeature = OpenFeature
  { providerRegistry :: Map.Map Text DynFeatureProvider,
    globalContext :: EvaluationContext
  }

-- | Global TVar for OpenFeature state
openFeatureTVar :: Conc.TVar OpenFeature
openFeatureTVar = unsafePerformIO $ Conc.newTVarIO $ OpenFeature mempty defaultContext
{-# NOINLINE openFeatureTVar #-}

setNamedProvider :: FeatureProvider p => Text -> p -> IO ()
setNamedProvider name provider = Conc.atomically $ do
  state <- Conc.readTVar openFeatureTVar
  let pr = Map.insert name (DynFeatureProvider provider) (providerRegistry state)
  Conc.writeTVar openFeatureTVar $ state {providerRegistry = pr}

setDefaultProvider :: FeatureProvider p => p -> IO ()
setDefaultProvider = setNamedProvider ""

setGlobalContext :: EvaluationContext -> IO ()
setGlobalContext ctx = Conc.atomically $ do
  state <- Conc.readTVar openFeatureTVar
  Conc.writeTVar openFeatureTVar $ state {globalContext = ctx}

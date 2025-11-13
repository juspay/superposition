module Data.OpenFeature.Client
  ( Client,
    clientName,
    clientContext,
    createClient,
    createNamedClient,
    setClientContext,
    getBoolValue,
    getStringValue,
    getIntValue,
    getDoubleValue,
    getObjectValue,
    getBoolDetails,
    getStringDetails,
    getIntDetails,
    getDoubleDetails,
    getObjectDetails,
  )
where

import Data.Aeson (Value)
import Data.Map qualified as Map
import Data.OpenFeature.Api
import Data.OpenFeature.EvaluationContext as EC
import Data.OpenFeature.EvaluationDetails as ED
import Data.OpenFeature.FeatureProvider as FP
import Data.Text (Text)
import GHC.Conc qualified as Conc
import Data.Maybe (fromMaybe)

-- | Client represents a named client with its own evaluation context
data Client = Client
  { clientName :: Text,
    clientContext :: EvaluationContext
  }

-- | Create a default unnamed client
createClient :: IO Client
createClient = return $ Client "" defaultContext

-- | Create a named client
createNamedClient :: Text -> IO Client
createNamedClient name = return $ Client name defaultContext

setClientContext :: EvaluationContext -> Client -> Client
setClientContext ec client = client { clientContext = ec }

toEvaluationDetails :: Text -> ResolutionDetails a -> EvaluationDetails a
toEvaluationDetails key res =
  EvaluationDetails
    { flagKey = key,
      value = FP.value res,
      variant = FP.variant res,
      reason = FP.reason res,
      flagMetadata = fromMaybe Map.empty (FP.flagMetadata res)
    }

-- | Get boolean flag value
getBoolValue ::
  Client ->
  Text ->
  Maybe EvaluationContext ->
  IO (EvaluationResult Bool)
getBoolValue client fkey ec =
  (ED.value <$>) <$> getBoolDetails client fkey ec

-- | Get string flag value
getStringValue ::
  Client ->
  Text ->
  Maybe EvaluationContext ->
  IO (EvaluationResult Text)
getStringValue client fkey ec =
  (ED.value <$>) <$> getStringDetails client fkey ec

-- | Get integer flag value
getIntValue ::
  Client ->
  Text ->
  Maybe EvaluationContext ->
  IO (EvaluationResult Integer)
getIntValue client fkey ec =
  (ED.value <$>) <$> getIntDetails client fkey ec

-- | Get double flag value
getDoubleValue ::
  Client ->
  Text ->
  Maybe EvaluationContext ->
  IO (EvaluationResult Double)
getDoubleValue client fkey ec =
  (ED.value <$>) <$> getDoubleDetails client fkey ec

-- | Get object flag value
getObjectValue ::
  Client ->
  Text ->
  Maybe EvaluationContext ->
  IO (EvaluationResult Value)
getObjectValue client fkey maybeCtx =
  (ED.value <$>) <$> getObjectDetails client fkey maybeCtx

-- | Get boolean flag with full evaluation details
getBoolDetails ::
  Client ->
  Text ->
  Maybe EvaluationContext ->
  IO (EvaluationResult (EvaluationDetails Bool))
getBoolDetails client fkey ec =
    resolveKey client fkey ec (\ (DynFeatureProvider p) -> FP.resolveBooleanValue p)

-- | Get string flag with full evaluation details
getStringDetails ::
  Client ->
  Text ->
  Maybe EvaluationContext ->
  IO (EvaluationResult (EvaluationDetails Text))
getStringDetails client fkey ec =
    resolveKey client fkey ec (\ (DynFeatureProvider p) -> FP.resolveStringValue p)

-- | Get integer flag with full evaluation details
getIntDetails ::
  Client ->
  Text ->
  Maybe EvaluationContext ->
  IO (EvaluationResult (EvaluationDetails Integer))
getIntDetails client fkey ec =
    resolveKey client fkey ec (\ (DynFeatureProvider p) -> FP.resolveIntegerValue p)

-- | Get double flag with full evaluation details
getDoubleDetails ::
  Client ->
  Text ->
  Maybe EvaluationContext ->
  IO (EvaluationResult (EvaluationDetails Double))
getDoubleDetails client fkey ec =
    resolveKey client fkey ec (\ (DynFeatureProvider p) -> FP.resolveDoubleValue p)

-- | Get object flag with full evaluation details
getObjectDetails ::
  Client ->
  Text ->
  Maybe EvaluationContext ->
  IO (EvaluationResult (EvaluationDetails Value))
getObjectDetails client fkey ec =
    resolveKey client fkey ec (\ (DynFeatureProvider p) -> FP.resolveObjectValue p)

resolveKey ::
    Client ->
    Text ->
    Maybe EvaluationContext ->
    (DynFeatureProvider -> Text -> EvaluationContext -> IO (EvaluationResult (ResolutionDetails a))) ->
    IO (EvaluationResult (EvaluationDetails a))
resolveKey client fkey ec resolveFn = do
    gstate <- Conc.readTVarIO openFeatureTVar
    let provider = Map.lookup (clientName client) (providerRegistry gstate)
        ctx = fromMaybe mempty ec <> clientContext client <> globalContext gstate
    case provider of
        Nothing -> return $ Left $ EvaluationError ProviderNotReady (Just "No provider set.")
        Just p -> do
            result <- resolveFn p fkey ctx
            return $ toEvaluationDetails fkey <$> result

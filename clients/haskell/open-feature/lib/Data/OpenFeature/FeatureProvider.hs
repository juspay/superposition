module Data.OpenFeature.FeatureProvider
  ( FeatureProvider (..),
    ResolutionDetails (..),
    ProviderMetadata (..),
    ProviderStatus (..),
    defaultResolution,
  )
where

import Data.Aeson (Value)
import Data.OpenFeature.EvaluationContext
import Data.OpenFeature.EvaluationDetails
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Details returned by a provider when resolving a flag
data ResolutionDetails a = ResolutionDetails
  { value :: a,
    variant :: Maybe Text,
    reason :: Maybe EvaluationReason,
    flagMetadata :: Maybe FlagMetadata
  }
  deriving (Show, Eq, Functor, Generic)

defaultResolution :: a -> ResolutionDetails a
defaultResolution val =
  ResolutionDetails
    { value = val,
      variant = Nothing,
      reason = Nothing,
      flagMetadata = Nothing
    }

data ProviderMetadata = ProviderMetadata
  { providerName :: Text
  }
  deriving (Show, Eq, Generic)

data ProviderStatus
  = -- | The provider has not been initialized.
    NotReady
  | -- | The provider has been initialized, and is able to reliably resolve flag values.
    Ready
  | -- | The provider is initialized but is not able to reliably resolve flag values.
    Error
  | -- | The provider's cached state is no longer valid and may not be
    --  up-to-date with the source truth.
    STALE
  deriving (Show, Eq, Generic)

-- | The core provider interface
class FeatureProvider p where
  getMetadata :: p -> ProviderMetadata

  getStatus :: p -> IO ProviderStatus

  initialize :: p -> EvaluationContext -> IO ()

  -- | Evaluate a boolean flag
  resolveBooleanValue ::
    p ->
    Text ->
    EvaluationContext ->
    IO (EvaluationResult (ResolutionDetails Bool))

  -- | Evaluate a string flag
  resolveStringValue ::
    p ->
    Text ->
    EvaluationContext ->
    IO (EvaluationResult (ResolutionDetails Text))

  -- | Evaluate an integer flag
  resolveIntegerValue ::
    p ->
    Text ->
    EvaluationContext ->
    IO (EvaluationResult (ResolutionDetails Integer))

  -- | Evaluate a double flag
  resolveDoubleValue ::
    p ->
    Text ->
    EvaluationContext ->
    IO (EvaluationResult (ResolutionDetails Double))

  -- | Evaluate an object flag
  resolveObjectValue ::
    p ->
    Text ->
    EvaluationContext ->
    IO (EvaluationResult (ResolutionDetails Value))

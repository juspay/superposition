module Data.OpenFeature.EvaluationDetails
  ( EvaluationResult,
    EvaluationDetails (..),
    EvaluationReason (..),
    EvaluationError (..),
    EvaluationErrorCode (..),
    FlagMetadata,
    FlagMetadataValue (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Metadata about a flag
type FlagMetadata = Map.Map Text FlagMetadataValue

-- | Values that can be stored in flag metadata
data FlagMetadataValue
  = MetaBool Bool
  | MetaInt Int
  | MetaDouble Double
  | MetaString Text
  deriving (Show, Eq, Generic)

instance ToJSON FlagMetadataValue

-- | Reason for how a flag value was resolved
data EvaluationReason
  = Static
  | Default
  | TargetingMatch
  | Split
  | Cached
  | Disabled
  | Unknown
  | Error
  | Other Text
  deriving (Show, Eq, Generic)

instance ToJSON EvaluationReason

-- | Error codes that can occur during flag evaluation
-- Based on OpenFeature Rust SDK implementation
data EvaluationErrorCode
  = -- | The value was resolved before the provider was initialized.
    ProviderNotReady
  | -- | The flag could not be found.
    FlagNotFound
  | -- | An error was encountered parsing data, such as a flag configuration.
    ParseError
  | -- | The type of the flag value does not match the expected type.
    TypeMismatch
  | -- | The provider requires a targeting key and one was not provided.
    TargetingKeyMissing
  | -- | The evaluation context does not meet provider requirements.
    InvalidContext
  | -- | The error was for a reason not enumerated above.
    General Text
  deriving (Show, Eq, Generic)

instance ToJSON EvaluationErrorCode

-- | Evaluation error with error code and optional message
data EvaluationError = EvaluationError
  { errorCode :: EvaluationErrorCode,
    message :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON EvaluationError

-- | Result of a flag evaluation
type EvaluationResult a = Either EvaluationError a

-- | Details about a flag evaluation result
data EvaluationDetails a = EvaluationDetails
  { flagKey :: Text,
    value :: a,
    reason :: Maybe EvaluationReason,
    variant :: Maybe Text,
    flagMetadata :: FlagMetadata
  }
  deriving (Show, Eq, Functor, Generic)

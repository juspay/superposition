module Data.OpenFeature.EvaluationContext
  ( EvaluationContext,
    targetingKey,
    customFields,
    defaultContext,
    contextWithTargetingKey,
    withTargetingKey,
    withCustomField,
    addCustomField,
    mergeEvaluationContext,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON, Value)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Context contains targeting information for flag evaluation
data EvaluationContext = EvaluationContext
  { targetingKey :: Maybe Text,
    customFields :: Map.Map Text Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON EvaluationContext

instance Semigroup EvaluationContext where
    (<>) = mergeEvaluationContext

instance Monoid EvaluationContext where
    mempty = defaultContext

-- | Create a default empty evaluation context
defaultContext :: EvaluationContext
defaultContext = EvaluationContext Nothing Map.empty

-- | Create a context with a targeting key
contextWithTargetingKey :: Text -> EvaluationContext
contextWithTargetingKey key = EvaluationContext (Just key) Map.empty

-- | Set the targeting key
withTargetingKey :: Text -> EvaluationContext -> EvaluationContext
withTargetingKey key ctx = ctx {targetingKey = Just key}

-- | Add a custom field
withCustomField :: Text -> Value -> EvaluationContext -> EvaluationContext
withCustomField key val ctx =
  ctx {customFields = Map.insert key val (customFields ctx)}

-- | Add a custom field directly
addCustomField :: Text -> Value -> EvaluationContext -> EvaluationContext
addCustomField key val ctx =
  ctx {customFields = Map.insert key val (customFields ctx)}

-- | Merge two evaluation contexts, with the first taking precedence
mergeEvaluationContext :: EvaluationContext -> EvaluationContext -> EvaluationContext
mergeEvaluationContext c1 c2 =
  EvaluationContext
    { targetingKey = targetingKey c1 <|> targetingKey c2,
      customFields = Map.union (customFields c1) (customFields c2)
    }

module Data.OpenFeature.Provider where

import Control.Applicative ((<|>))
import Data.Aeson (Value, ToJSON)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Context contains targeting information for flag evaluation
data EvaluationContext = EvaluationContext
  { targetingKey :: Maybe Text,
    attributes :: Map.Map Text Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON EvaluationContext

mergeEvaluationContext :: EvaluationContext -> EvaluationContext -> EvaluationContext
mergeEvaluationContext ctx1 ctx2 =
  EvaluationContext
    { targetingKey = targetingKey ctx2 <|> targetingKey ctx1,
      attributes = Map.union (attributes ctx2) (attributes ctx1)
    }

-- | Provider error types
data ProviderError
  = FlagNotFound Text
  | TypeMismatch Text
  | ProviderNotReady Text
  | GeneralError Text
  deriving (Show, Eq, Generic)

instance ToJSON ProviderError

-- | Details about how a flag value was resolved
data ResolutionDetails a = ResolutionDetails
  { value :: a,
    variant :: Text,
    reason :: Text,
    -- metadata :: Map.Map Text Value,
    errorCode :: Maybe ProviderError
  }
  deriving (Show, Eq, Functor)

-- | The core provider interface
class Provider p where
  -- | Get provider metadata
  getMetadata :: p -> Map.Map Text Value

  -- | Initialize the provider
  initialize :: p -> EvaluationContext -> IO (Either Text ())

  -- | Evaluate a boolean flag
  resolveBooleanValue ::
    p ->
    Text ->
    Bool ->
    EvaluationContext ->
    IO (ResolutionDetails Bool)

  -- | Evaluate a string flag
  resolveStringValue ::
    p ->
    Text ->
    Text ->
    EvaluationContext ->
    IO (ResolutionDetails Text)

  -- | Evaluate an integer flag
  resolveIntegerValue ::
    p ->
    Text ->
    Integer ->
    EvaluationContext ->
    IO (ResolutionDetails Integer)

  -- | Evaluate an integer flag
  resolveDoubleValue ::
    p ->
    Text ->
    Double ->
    EvaluationContext ->
    IO (ResolutionDetails Double)

  -- | Evaluate an object flag
  resolveObjectValue ::
    p ->
    Text ->
    Value ->
    EvaluationContext ->
    IO (ResolutionDetails Value)

-- | Helper function to create default context
defaultContext :: EvaluationContext
defaultContext = EvaluationContext Nothing Map.empty

-- | Helper function to create context with targeting key
contextWithKey :: Text -> EvaluationContext
contextWithKey key = EvaluationContext (Just key) Map.empty

-- | Helper function to create successful resolution
success :: a -> ResolutionDetails a
success val =
  ResolutionDetails
    { value = val,
      variant = "",
      reason = "",
      errorCode = Nothing
      -- metadata = Map.empty
    }

-- | Helper function to create default resolution
defaultResolution :: a -> ResolutionDetails a
defaultResolution val =
  ResolutionDetails
    { value = val,
      variant = "static",
      reason = "",
      errorCode = Nothing
      -- metadata = Map.empty
    }

-- -- Example implementation of a simple in-memory provider
-- data InMemoryProvider = InMemoryProvider
--   { boolFlags :: Map.Map Text Bool,
--     stringFlags :: Map.Map Text Text,
--     numberFlags :: Map.Map Text Double,
--     objectFlags :: Map.Map Text Value
--   }
--   deriving (Show)

-- instance Provider InMemoryProvider where
--   getMetadata _ = Map.fromList [("name", "in-memory-provider")]

--   initialize _ _ = return $ Right ()

--   resolveBooleanValue provider flagKey defaultVal _ctx = do
--     case Map.lookup flagKey (boolFlags provider) of
--       Just val -> return $ success val
--       Nothing -> return $ defaultResolution defaultVal

--   resolveStringValue provider flagKey defaultVal _ctx = do
--     case Map.lookup flagKey (stringFlags provider) of
--       Just val -> return $ success val
--       Nothing -> return $ defaultResolution defaultVal

--   resolveNumberValue provider flagKey defaultVal _ctx = do
--     case Map.lookup flagKey (numberFlags provider) of
--       Just val -> return $ success val
--       Nothing -> return $ defaultResolution defaultVal

--   resolveObjectValue provider flagKey defaultVal _ctx = do
--     case Map.lookup flagKey (objectFlags provider) of
--       Just val -> return $ success val
--       Nothing -> return $ defaultResolution defaultVal

-- -- | Create an empty in-memory provider
-- emptyProvider :: InMemoryProvider
-- emptyProvider = InMemoryProvider Map.empty Map.empty Map.empty Map.empty

-- -- | Add a boolean flag to the provider
-- withBoolFlag :: Text -> Bool -> InMemoryProvider -> InMemoryProvider
-- withBoolFlag key val provider =
--   provider {boolFlags = Map.insert key val (boolFlags provider)}

-- -- | Add a string flag to the provider
-- withStringFlag :: Text -> Text -> InMemoryProvider -> InMemoryProvider
-- withStringFlag key val provider =
--   provider {stringFlags = Map.insert key val (stringFlags provider)}

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FFI.Superposition
  ( getResolvedConfig,
    ResolveConfigParams (..),
    defaultResolveParams,
    MergeStrategy (..),
    parseTomlConfig,
    parseJsonConfig,
    -- ProviderCache API
    ProviderCacheHandle,
    newProviderCache,
    freeProviderCache,
    initConfig,
    initExperiments,
    evalConfig,
  ) where

import Control.Monad (when)
import Data.Aeson (Value, eitherDecodeStrict')
import Data.ByteString (packCString)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Foreign (callocBytes, nullPtr)
import Foreign.C.String (CString, newCString, peekCAString)
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Marshal (free)

type BufferSize = Int
errorBufferSize :: BufferSize
errorBufferSize = 2048

data ProviderCacheOpaque
type ProviderCacheHandle = Ptr ProviderCacheOpaque

foreign import capi "superposition_core.h core_provider_cache_new"
  provider_cache_new :: IO ProviderCacheHandle

foreign import capi "superposition_core.h core_provider_cache_free"
  provider_cache_free :: ProviderCacheHandle -> IO ()

foreign import capi "superposition_core.h core_provider_cache_init_config"
  provider_cache_init_config ::
    ProviderCacheHandle ->
    CString -> -- default_config_json
    CString -> -- contexts_json
    CString -> -- overrides_json
    CString -> -- dimensions_json
    CString -> -- ebuf
    IO ()

foreign import capi "superposition_core.h core_provider_cache_init_experiments"
  provider_cache_init_experiments ::
    ProviderCacheHandle ->
    CString -> -- experiments_json
    CString -> -- experiment_groups_json
    CString -> -- ebuf
    IO ()

foreign import capi "superposition_core.h core_provider_cache_eval_config"
  provider_cache_eval_config ::
    ProviderCacheHandle ->
    CString -> -- query_data_json
    CString -> -- merge_strategy_str
    CString -> -- filter_prefixes_json (nullable)
    CString -> -- targeting_key (nullable)
    CString -> -- ebuf
    IO CString

foreign import capi "superposition_core.h core_get_resolved_config"
  get_resolved_config ::
    -- | default_config_json
    CString ->
    -- | context_json
    CString ->
    -- | overrides_json
    CString ->
    -- | dimension_info_json
    CString ->
    -- | query_data_json
    CString ->
    -- | merge_strategy_json
    CString ->
    -- | filter_prefixes_json (optional)
    CString ->
    -- | experimentation_json (optional)
    CString ->
    -- | error-buffer
    CString ->
    -- | resolved config json
    IO CString

foreign import capi "superposition_core.h core_parse_toml_config"
  parse_toml_config ::
    -- | toml_content
    CString ->
    -- | error-buffer
    CString ->
    -- | parsed config json
    IO CString

foreign import capi "superposition_core.h core_parse_json_config"
  parse_json_config ::
    -- | json_content
    CString ->
    -- | error-buffer
    CString ->
    -- | parsed config json
    IO CString

foreign import capi "superposition_core.h &core_free_string"
  p_free_string :: FunPtr (CString -> IO ())

data MergeStrategy = Merge | Replace

instance Show MergeStrategy where
  show Merge = "MERGE"
  show Replace = "REPLACE"

-- | Create a new ProviderCache handle.
newProviderCache :: IO ProviderCacheHandle
newProviderCache = provider_cache_new

-- | Free a ProviderCache handle.
freeProviderCache :: ProviderCacheHandle -> IO ()
freeProviderCache = provider_cache_free

-- | Initialize the config data in the cache.
-- Returns Nothing on success, Just errorMsg on failure.
initConfig :: ProviderCacheHandle -> String -> String -> String -> String -> IO (Maybe String)
initConfig handle defaultCfg contexts overrides dimensions = do
  ebuf <- callocBytes errorBufferSize
  dcStr <- newCString defaultCfg
  ctxStr <- newCString contexts
  ovStr <- newCString overrides
  dimStr <- newCString dimensions
  provider_cache_init_config handle dcStr ctxStr ovStr dimStr ebuf
  err <- peekCAString ebuf
  traverse_ free [dcStr, ctxStr, ovStr, dimStr, ebuf]
  pure $ if null err then Nothing else Just err

-- | Initialize experiments data in the cache.
-- Returns Nothing on success, Just errorMsg on failure.
initExperiments :: ProviderCacheHandle -> String -> String -> IO (Maybe String)
initExperiments handle experiments experimentGroups = do
  ebuf <- callocBytes errorBufferSize
  expStr <- newCString experiments
  egStr <- newCString experimentGroups
  provider_cache_init_experiments handle expStr egStr ebuf
  err <- peekCAString ebuf
  traverse_ free [expStr, egStr, ebuf]
  pure $ if null err then Nothing else Just err

-- | Evaluate config from the cache.
evalConfig :: ProviderCacheHandle -> String -> MergeStrategy -> Maybe String -> Maybe String -> IO (Either String String)
evalConfig handle queryJson strategy filterPrefixes targetingKey = do
  ebuf <- callocBytes errorBufferSize
  qStr <- newCString queryJson
  msStr <- newCString (show strategy)
  fpStr <- maybe (pure nullPtr) newCString filterPrefixes
  tkStr <- maybe (pure nullPtr) newCString targetingKey
  res <- provider_cache_eval_config handle qStr msStr fpStr tkStr ebuf
  err <- peekCAString ebuf
  result <- if res /= nullPtr
    then Just <$> peekCAString res
    else pure Nothing
  -- Free result string via core_free_string
  when (res /= nullPtr) $ do
    resFptr <- newForeignPtr p_free_string res
    withForeignPtr resFptr (\_ -> pure ())
  let freeNonNull p = when (p /= nullPtr) (free p)
  traverse_ freeNonNull [qStr, msStr, fpStr, tkStr, ebuf]
  pure $ case (result, err) of
    (Just cfg, []) -> Right cfg
    (Nothing, []) -> Left "null pointer returned"
    _ -> Left err

-- Legacy stateless API

data ResolveConfigParams = ResolveConfigParams
  { defaultConfig :: Maybe String,
    context :: Maybe String,
    overrides :: Maybe String,
    dimensionInfo :: Maybe String,
    query :: Maybe String,
    mergeStrategy :: Maybe MergeStrategy,
    prefixFilter :: Maybe String,
    experimentation :: Maybe String
  }

defaultResolveParams :: ResolveConfigParams
defaultResolveParams =
  ResolveConfigParams
    { defaultConfig = Nothing,
      context = Nothing,
      overrides = Nothing,
      dimensionInfo = Nothing,
      query = Nothing,
      mergeStrategy = Nothing,
      prefixFilter = Nothing,
      experimentation = Nothing
    }

getResolvedConfig :: ResolveConfigParams -> IO (Either String String)
getResolvedConfig params = do
  ebuf <- callocBytes errorBufferSize
  let ResolveConfigParams {..} = params
      newOrNull = maybe (pure nullPtr) newCString
      freeNonNull p = when (p /= nullPtr) (free p)
      peekMaybe p | p /= nullPtr = Just <$> peekCAString p
                  | otherwise = pure Nothing
  dc <- newOrNull defaultConfig
  ctx <- newOrNull context
  ovrs <- newOrNull overrides
  di <- newOrNull dimensionInfo
  qry <- newOrNull query
  mergeS <- newCString $ show $ fromMaybe Merge mergeStrategy
  pfltr <- newOrNull prefixFilter
  exp <- newOrNull experimentation
  res <-
    peekMaybe
      =<< get_resolved_config
        dc
        ctx
        ovrs
        di
        qry
        mergeS
        pfltr
        exp
        ebuf
  err <- peekCAString ebuf
  traverse_ freeNonNull [dc, ctx, ovrs, di, qry, mergeS, pfltr, exp, ebuf]
  pure $ case (res, err) of
    (Just cfg, []) -> Right cfg
    (Nothing, []) -> Left "null pointer returned"
    _ -> Left err

-- | Parse TOML configuration string into structured format
-- Returns JSON matching the Config type with:
--   - contexts: array of context objects with id, condition, priority, weight, override_with_keys
--   - overrides: object mapping override IDs to override key-value pairs
--   - default_configs: object with configuration key-value pairs
--   - dimensions: object mapping dimension names to dimension info (schema, position, etc.)
parseTomlConfig :: String -> IO (Either String Value)
parseTomlConfig tomlContent = do
  ebuf <- callocBytes errorBufferSize
  tomlStr <- newCString tomlContent
  res <- parse_toml_config tomlStr ebuf
  errBytes <- packCString ebuf
  let errText = fromRight mempty $ decodeUtf8' errBytes
  result <- if res /= nullPtr
    then do
      resFptr <- newForeignPtr p_free_string res
      -- Registers p_free_string as the finalizer (for automatic cleanup)
      withForeignPtr resFptr $ fmap Just . packCString
    else pure Nothing
  free tomlStr
  free ebuf
  pure $ case (result, errText) of
    (Just cfg, t) | T.null t -> case eitherDecodeStrict' cfg of
      Right val -> Right val
      Left e    -> Left $ "JSON parse error: " ++ e
    (Nothing, t) | T.null t -> Left "null pointer returned"
    _ -> Left (unpack errText)

-- | Parse JSON configuration string into structured format
-- Returns JSON matching the Config type with:
--   - contexts: array of context objects with id, condition, priority, weight, override_with_keys
--   - overrides: object mapping override IDs to override key-value pairs
--   - default_configs: object with configuration key-value pairs
--   - dimensions: object mapping dimension names to dimension info (schema, position, etc.)
parseJsonConfig :: String -> IO (Either String Value)
parseJsonConfig jsonContent = do
  ebuf <- callocBytes errorBufferSize
  jsonStr <- newCString jsonContent
  res <- parse_json_config jsonStr ebuf
  errBytes <- packCString ebuf
  let errText = fromRight mempty $ decodeUtf8' errBytes
  result <- if res /= nullPtr
    then do
      resFptr <- newForeignPtr p_free_string res
      withForeignPtr resFptr $ fmap Just . packCString
    else pure Nothing
  free jsonStr
  free ebuf
  pure $ case (result, errText) of
    (Just cfg, t) | T.null t -> case eitherDecodeStrict' cfg of
      Right val -> Right val
      Left e    -> Left $ "JSON parse error: " ++ e
    (Nothing, t) | T.null t -> Left "null pointer returned"
    _ -> Left (unpack errText)

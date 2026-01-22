{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FFI.Superposition (getResolvedConfig, ResolveConfigParams (..), defaultResolveParams, MergeStrategy (..), parseTomlConfig) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Foreign (callocBytes, nullPtr)
import Foreign.C.String (CString, newCString, peekCAString)
import Foreign.Marshal (free)
import Prelude


foreign import capi "superposition_core.h core_get_resolved_config"
  get_resolved_config ::
    -- | default_config_json
    CString ->
    -- | context_json
    CString ->
    -- | overrides_json
    CString ->
    -- | dimenson_info_json
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

data MergeStrategy = Merge | Replace

instance Show MergeStrategy where
  show Merge = "MERGE"
  show Replace = "REPLACE"

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
  ebuf <- callocBytes 2048
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
  traverse_ freeNonNull [dc, ctx, ovrs, qry, mergeS, pfltr, exp, ebuf]
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
parseTomlConfig :: String -> IO (Either String String)
parseTomlConfig tomlContent = do
  ebuf <- callocBytes 2048  -- Error buffer size matches Rust implementation
  tomlStr <- newCString tomlContent
  res <- parse_toml_config tomlStr ebuf
  err <- peekCAString ebuf
  let peekMaybe p | p /= nullPtr = Just <$> peekCAString p
                  | otherwise = pure Nothing
  result <- peekMaybe res
  when (res /= nullPtr) (free res)
  free tomlStr
  free ebuf
  pure $ case (result, err) of
    (Just cfg, []) -> Right cfg
    (Nothing, []) -> Left "null pointer returned"
    _ -> Left err

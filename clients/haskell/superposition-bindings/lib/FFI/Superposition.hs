{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FFI.Superposition (getResolvedConfig, ResolveConfigParams (..), defaultResolveParams, MergeStrategy (..)) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Foreign (callocBytes, nullPtr)
import Foreign.C.String (CString, newCString, peekCAString)
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

foreign import capi "superposition_core.h core_free_string"
  free_string :: CString -> IO ()

foreign import capi "superposition_core.h core_get_applicable_variants"
    get_applicable_variants ::
      CString -> -- | experiments_json
      CString -> -- | experiment_groups_json
      CString -> -- | dimensions_json
      CString -> -- | query_data_json
      CString -> -- | targeting_key
      CString -> -- | filter_prefixes_json
      CString -> -- | error-buffer
      IO CString -- | applicable variants json

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

data GetApplicableVariantsParams = GetApplicableVariantsParams
  { experiments :: Maybe String,
    experimentGroups :: Maybe String,
    dimensions :: Maybe String,
    queryData :: Maybe String,
    targetingKey :: Maybe String,
    filterPrefixes :: Maybe String
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

defaultApplicableVariantsParams :: GetApplicableVariantsParams
defaultApplicableVariantsParams =
  GetApplicableVariantsParams
    { experiments = Nothing,
      experimentGroups = Nothing,
      dimensions = Nothing,
      queryData = Nothing,
      targetingKey = Nothing,
      filterPrefixes = Nothing
    }    

getResolvedConfig :: ResolveConfigParams -> IO (Either String String)
getResolvedConfig params = do
  ebuf <- callocBytes 256
  let ResolveConfigParams {..} = params
      newOrNull = maybe (pure nullPtr) newCString
      freeNonNull p = when (p /= nullPtr) (free_string p)
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

getApplicableVariants ::
  GetApplicableVariantsParams ->
  IO (Either String String)
getApplicableVariants GetApplicableVariantsParams {..} = do
  ebuf <- callocBytes 256
  let newOrNull = maybe (pure nullPtr) newCString
      freeNonNull p = when (p /= nullPtr) (free_string p)
      peekMaybe p | p /= nullPtr = Just <$> peekCAString p
                  | otherwise = pure Nothing
  exps <- newOrNull experiments
  expGrps <- newOrNull experimentGroups
  dims <- newOrNull dimensions
  qry <- newOrNull queryData
  tkey <- newOrNull targetingKey
  pfltr <- newOrNull filterPrefixes
  res <-
    peekMaybe
      =<< get_applicable_variants
        exps
        expGrps
        dims
        qry
        tkey
        pfltr
        ebuf
  err <- peekCAString ebuf
  traverse_ freeNonNull [exps, expGrps, dims, qry, tkey, pfltr, ebuf]
  pure $ case (res, err) of
    (Just vars, []) -> Right vars
    (Nothing, []) -> Left "null pointer returned"
    _ -> Left err

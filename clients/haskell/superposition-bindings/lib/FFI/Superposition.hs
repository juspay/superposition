{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FFI.Superposition (getResolvedConfig, ResolveConfigParams (..), defaultResolveParams, MergeStrategy (..), parseTomlConfig) where

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
import Foreign.Ptr (FunPtr)
import Foreign.Marshal (free)

type BufferSize = Int 
errorBufferSize :: BufferSize 
errorBufferSize = 2048

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

foreign import capi "superposition_core.h &core_free_string"
  p_free_string :: FunPtr (CString -> IO ())

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
      withForeignPtr resFptr $ \ptr -> Just <$> packCString ptr
    else pure Nothing
  free tomlStr
  free ebuf
  pure $ case (result, errText) of
    (Just cfg, t) | T.null t -> case eitherDecodeStrict' cfg of
      Right val -> Right val
      Left e    -> Left $ "JSON parse error: " ++ e
    (Nothing, t) | T.null t -> Left "null pointer returned"
    _ -> Left (unpack errText)

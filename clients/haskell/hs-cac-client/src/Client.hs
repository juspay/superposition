{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Client
( createCacClient
, getCacClient
, getFullConfigStateWithFilter
, getCacLastModified
, getResolvedConfig
, cacStartPolling
, getDefaultConfig
, getResolvedConfigWithStrategy
, MergeStrategy (..)
) where

import           Data.Aeson
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Functor          (($>))
import           Data.List             (intercalate)
import           Data.Text (pack)
import           Foreign.C.String      (CString, newCString, peekCString)
import           Foreign.C.Types       (CInt (CInt), CULong (..))
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc (malloc, free)
import           Foreign.Marshal.Array (withArrayLen)
import           Foreign.Ptr
import           Prelude

data Arc_Client

type CacClient = Arc_Client

type CTenant = CString
type Tenant = String

type Error = String

foreign import ccall unsafe "cac_new_client"
    c_new_cac_client :: CTenant -> CULong -> CString -> IO CInt

foreign import ccall unsafe "cac_new_client_with_cache_properties"
    c_new_cac_client_with_cache_properties :: CTenant -> CULong -> CString -> CULong -> CULong -> CULong -> IO CInt

foreign import ccall unsafe "&cac_free_client"
    c_free_cac_client :: FunPtr (Ptr CacClient -> IO ())

foreign import ccall unsafe "cac_get_client"
    c_get_cac_client :: CTenant -> IO (Ptr CacClient)

foreign import ccall unsafe "cac_last_error_message"
    c_last_error_message :: IO CString

foreign import ccall unsafe "cac_get_last_modified"
    c_get_last_modified_time :: Ptr CacClient -> IO CString

foreign import ccall unsafe "cac_get_config"
    c_get_config :: Ptr CacClient -> CString -> CString -> IO CString

foreign import ccall unsafe "cac_get_resolved_config"
    c_cac_get_resolved_config :: Ptr CacClient -> CString -> CString -> CString -> IO CString

foreign import ccall unsafe "cac_get_default_config"
    c_cac_get_default_config :: Ptr CacClient -> CString -> IO CString

foreign import ccall safe "cac_start_polling_update"
    c_cac_poll :: CTenant -> IO ()

foreign import ccall unsafe "&cac_free_string"
    c_free_string :: FunPtr (CString -> IO ())

data MergeStrategy = MERGE | REPLACE deriving (Show, Eq, Ord, Enum)

cacStartPolling :: Tenant -> IO ()
cacStartPolling tenant =
    newCString tenant
    >>= newForeignPtr c_free_string
    >>= flip withForeignPtr c_cac_poll

getError :: IO String
getError = c_last_error_message
            >>= newForeignPtr c_free_string
            >>= flip withForeignPtr peekCString

cleanup :: [Ptr a] -> IO ()
cleanup items = mapM free items $> ()

createCacClient:: Tenant -> Integer -> String -> IO (Either Error ())
createCacClient tenant frequency hostname = do
    let duration = fromInteger frequency
    cTenant   <- newCString tenant
    cHostname <- newCString hostname
    resp      <- c_new_cac_client cTenant duration cHostname
    _         <- cleanup [cTenant, cHostname]
    case resp of
        0 -> pure $ Right ()
        _ -> Left <$> getError

createCacClientWithCacheProperties:: Tenant -> Integer -> String -> Integer -> Integer -> Integer -> IO (Either Error ())
createCacClientWithCacheProperties tenant frequency hostname cacheMaxCapacity cacheTTL cacheTTI = do
    let duration = fromInteger frequency
    let cacheCapacity = fromInteger cacheMaxCapacity
    let cacheTimeToLive = fromInteger cacheTTL
    let cacheTimeToIdle = fromInteger cacheTTI
    cTenant   <- newCString tenant
    cHostname <- newCString hostname
    resp      <- c_new_cac_client_with_cache_properties cTenant duration cHostname cacheCapacity cacheTimeToLive cacheTimeToIdle
    _         <- cleanup [cTenant, cHostname]
    case resp of
        0 -> pure $ Right ()
        _ -> Left <$> getError

getCacClient :: Tenant -> IO (Either Error (ForeignPtr CacClient))
getCacClient tenant = do
    cTenant   <- newCString tenant
    cacClient <- c_get_cac_client cTenant
    _         <- cleanup [cTenant]
    if cacClient == nullPtr
        then Left <$> getError
        else Right <$> newForeignPtr c_free_cac_client cacClient

getFullConfigStateWithFilter :: ForeignPtr CacClient -> Maybe String -> Maybe [String] -> IO (Either Error Value)
getFullConfigStateWithFilter client mbFilters mbPrefix = do
    cFilters <- case mbFilters of
        Just filters -> newCString filters
        Nothing      -> return nullPtr
    cPrefix <- case mbPrefix of
        Just prefix -> newCString (intercalate "," prefix)
        Nothing     -> return nullPtr
    config <- withForeignPtr client $ \clientPtr -> c_get_config clientPtr cFilters cPrefix
    _ <- cleanup [cFilters]
    if config == nullPtr
        then Left <$> getError
        else do
            fptrConfig <- newForeignPtr c_free_string config
            (maybe (Left "Failed to decode resolved config") Right . decodeStrict <$> encodeUtf8) . pack <$> withForeignPtr fptrConfig peekCString

getCacLastModified :: ForeignPtr CacClient -> IO (Either Error String)
getCacLastModified client = do
    lastModified <- withForeignPtr client c_get_last_modified_time
    if lastModified == nullPtr
        then Left <$> getError
        else do
            fptrLastModified <- newForeignPtr c_free_string lastModified
            Right <$> withForeignPtr fptrLastModified peekCString

getResolvedConfigWithStrategy :: FromJSON a => ForeignPtr CacClient -> String -> Maybe [String] -> MergeStrategy -> IO (Either Error a)
getResolvedConfigWithStrategy client context mbKeys mergeStrat = do
    cContext    <- newCString context
    cMergeStrat <- newCString (show mergeStrat)
    cStrKeys    <- case mbKeys of
        Just keys ->  newCString (intercalate "|" keys)
        Nothing   ->  return nullPtr
    overrides   <- withForeignPtr client $ \clientPtr -> c_cac_get_resolved_config clientPtr cContext cStrKeys cMergeStrat
    _           <- cleanup [cContext, cStrKeys]
    if overrides == nullPtr
        then Left <$> getError
        else do
            fptrOverrides <- newForeignPtr c_free_string overrides
            (maybe (Left "Failed to decode resolved config") Right . decodeStrict <$> encodeUtf8) . pack <$> withForeignPtr fptrOverrides peekCString


getDefaultConfig :: ForeignPtr CacClient -> Maybe [String] -> IO (Either Error Value)
getDefaultConfig client mbKeys = do
    cStrKeys    <- case mbKeys of
        Just keys ->  newCString (intercalate "|" keys)
        Nothing   ->  return nullPtr
    overrides   <- withForeignPtr client $ \clientPtr -> c_cac_get_default_config clientPtr cStrKeys
    _           <- cleanup [cStrKeys]
    if overrides == nullPtr
        then Left <$> getError
        else do
            fptrOverrides <- newForeignPtr c_free_string overrides
            Right . toJSON <$> withForeignPtr fptrOverrides peekCString

getResolvedConfig :: FromJSON a => ForeignPtr CacClient -> String -> Maybe [String] -> IO (Either Error a)
getResolvedConfig client context mbKeys = getResolvedConfigWithStrategy client context mbKeys MERGE

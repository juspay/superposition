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
) where

import           Data.Aeson
import           Data.Functor          (($>))
import           Data.List             (intercalate)
import           Foreign.C.String      (CString, newCAString, peekCAString)
import           Foreign.C.Types       (CInt (CInt), CULong (..))
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc (free)
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
    newCAString tenant
    >>= newForeignPtr c_free_string
    >>= flip withForeignPtr c_cac_poll

getError :: IO String
getError = c_last_error_message
            >>= newForeignPtr c_free_string
            >>= flip withForeignPtr peekCAString

cleanup :: [Ptr a] -> IO ()
cleanup items = mapM free items $> ()

createCacClient:: Tenant -> Integer -> String -> IO (Either Error ())
createCacClient tenant frequency hostname = do
    let duration = fromInteger frequency
    cTenant   <- newCAString tenant
    cHostname <- newCAString hostname
    resp      <- c_new_cac_client cTenant duration cHostname
    _         <- cleanup [cTenant, cHostname]
    case resp of
        0 -> pure $ Right ()
        _ -> Left <$> getError

getCacClient :: Tenant -> IO (Either Error (ForeignPtr CacClient))
getCacClient tenant = do
    cTenant   <- newCAString tenant
    cacClient <- c_get_cac_client cTenant
    _         <- cleanup [cTenant]
    if cacClient == nullPtr
        then Left <$> getError
        else Right <$> newForeignPtr c_free_cac_client cacClient

getFullConfigStateWithFilter :: ForeignPtr CacClient -> Maybe String -> Maybe String -> IO (Either Error Value)
getFullConfigStateWithFilter client mbFilters mbPrefix = do
    cFilters <- case mbFilters of
        Just filters -> newCAString filters
        Nothing      -> return nullPtr
    cPrefix <- case mbPrefix of
        Just prefix -> newCAString prefix
        Nothing     -> return nullPtr
    config <- withForeignPtr client $ \client -> c_get_config client cFilters cPrefix
    _ <- cleanup [cFilters]
    if config == nullPtr
        then Left <$> getError
        else do
            fptrConfig <- newForeignPtr c_free_string config
            Right . toJSON <$> withForeignPtr fptrConfig peekCAString

getCacLastModified :: ForeignPtr CacClient -> IO (Either Error String)
getCacLastModified client = do
    lastModified <- withForeignPtr client c_get_last_modified_time
    if lastModified == nullPtr
        then Left <$> getError
        else do
            fptrLastModified <- newForeignPtr c_free_string lastModified
            Right <$> withForeignPtr fptrLastModified peekCAString

getResolvedConfigWithStrategy :: ForeignPtr CacClient -> String -> Maybe [String] -> MergeStrategy -> IO (Either Error Value)
getResolvedConfigWithStrategy client context mbKeys mergeStrat = do
    cContext    <- newCAString context
    cMergeStrat <- newCAString (show mergeStrat)
    cStrKeys    <- case mbKeys of
        Just keys ->  newCAString (intercalate "|" keys)
        Nothing   ->  return nullPtr
    overrides   <- withForeignPtr client $ \client -> c_cac_get_resolved_config client cContext cStrKeys cMergeStrat
    _           <- cleanup [cContext, cStrKeys]
    if overrides == nullPtr
        then Left <$> getError
        else do
            fptrOverrides <- newForeignPtr c_free_string overrides
            Right . toJSON <$> withForeignPtr fptrOverrides peekCAString

getDefaultConfig :: ForeignPtr CacClient -> Maybe [String] -> IO (Either Error Value)
getDefaultConfig client mbKeys = do
    cStrKeys    <- case mbKeys of
        Just keys ->  newCAString (intercalate "|" keys)
        Nothing   ->  return nullPtr
    overrides   <- withForeignPtr client $ \client -> c_cac_get_default_config client cStrKeys
    _           <- cleanup [cStrKeys]
    if overrides == nullPtr
        then Left <$> getError
        else do
            fptrOverrides <- newForeignPtr c_free_string overrides
            Right . toJSON <$> withForeignPtr fptrOverrides peekCAString

getResolvedConfig :: ForeignPtr CacClient -> String -> Maybe [String] -> IO (Either Error Value)
getResolvedConfig client context mbKeys = getResolvedConfigWithStrategy client context mbKeys MERGE


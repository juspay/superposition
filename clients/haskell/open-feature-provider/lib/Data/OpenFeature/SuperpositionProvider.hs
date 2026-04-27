{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.OpenFeature.SuperpositionProvider
  ( defaultProviderOptions,
    SuperpositionProviderOptions (..),
    RefreshOptions (..),
    Log.LogLevel (..),
    newSuperpositionProvider,
    SuperpositionProvider,
    resolveAllConfig,
    closeSuperpositionProvider,
  )
where

import Control.Exception (SomeException, try)
import Control.Exception.Base (displayException)
import Control.Monad.Logger (LoggingT, filterLogger, runStdoutLoggingT)
import Control.Monad.Logger.Aeson qualified as Log
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON (..), encode, withObject, (.:?))
import Data.Aeson.Decoding (eitherDecode)
import Data.Aeson.Types (Object, parseEither)
import Data.Functor
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Time.Clock (UTCTime)
import Data.OpenFeature.FeatureProvider
import Data.OpenFeature.EvaluationDetails
import Data.OpenFeature.EvaluationContext
import Data.OpenFeature.SuperpositionProvider.OnDemandRefresh
import Data.OpenFeature.SuperpositionProvider.PollingRefresh
import Data.OpenFeature.SuperpositionProvider.RefreshTask (RefreshFn, RefreshTask (..))
import Data.OpenFeature.SuperpositionProviderOptions
import Data.String (IsString (..))
import Data.Text as T (Text, unpack)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import FFI.Superposition qualified as FFI
import GHC.Base (when)
import GHC.Conc (TVar, atomically, newTVarIO, readTVarIO)
import GHC.Conc.Sync (writeTVar)
import Io.Superposition.Command.GetConfig qualified as SDK
import Io.Superposition.Command.ListExperiment qualified as Exp
import Io.Superposition.Command.ListExperimentGroups qualified as ExpGrp
import Io.Superposition.Model.ExperimentStatusType (ExperimentStatusType (INPROGRESS, CREATED))
import Io.Superposition.Model.GetConfigInput qualified as SDK
import Io.Superposition.Model.GetConfigOutput qualified as SDK
import Io.Superposition.Model.ListExperimentInput qualified as Exp
import Io.Superposition.Model.ListExperimentOutput qualified as Exp
import Io.Superposition.Model.ListExperimentGroupsInput qualified as ExpGrp
import Io.Superposition.Model.ListExperimentGroupsOutput qualified as ExpGrp
import Io.Superposition.SuperpositionClient qualified as Client
import Network.HTTP.Client.TLS qualified as Net
import System.Mem.Weak (addFinalizer)

data DynRefreshTask a = forall r. (RefreshTask r a) => DynRefreshTask (r a)

type ConfigRefreshTask = DynRefreshTask SDK.GetConfigOutput

type ExperimentsRefreshTask = DynRefreshTask Exp.ListExperimentOutput

type ExperimentGroupsRefreshTask = DynRefreshTask ExpGrp.ListExperimentGroupsOutput

type Logger = forall a. LoggingT IO a -> IO a

data SuperpositionProvider = SuperpositionProvider
  { providerCache :: FFI.ProviderCacheHandle,
    configRefreshTask :: ConfigRefreshTask,
    expRefreshTask :: Maybe ExperimentsRefreshTask,
    expGrpRefreshTask :: Maybe ExperimentGroupsRefreshTask,
    _initContext :: TVar (Maybe EvaluationContext),
    _anchor :: IORef (),
    _cacheFreed :: IORef Bool,
    runLogger :: Logger,
    -- TODO
    fallbackConfig :: ()
  }

toStr :: (ToJSON a) => a -> String
toStr = LT.unpack . LTE.decodeUtf8 . encode

reinitCache :: FFI.ProviderCacheHandle -> Logger -> IORef (Maybe UTCTime) -> TVar (Maybe SDK.GetConfigOutput) -> IO ()
reinitCache cache logger lastModifiedRef configVar = do
  cfg <- readTVarIO configVar
  case cfg of
    Nothing -> pure ()
    Just config -> do
      prev <- readIORef lastModifiedRef
      let cur = SDK.last_modified config
      if prev == Just cur
        then logger $ Log.logDebug "Config unchanged (same last_modified), skipping reinit."
        else do
          let dc = toStr $ SDK.default_configs config
              ctx = toStr $ SDK.contexts config
              ovrs = toStr $ SDK.overrides config
              dims = toStr $ SDK.dimensions config
          result <- FFI.initConfig cache dc ctx ovrs dims
          case result of
            Nothing -> do
              writeIORef lastModifiedRef (Just cur)
              logger $ Log.logDebug "ProviderCache config reinitialised."
            Just err -> logger $ Log.logError $ fromString $ "ProviderCache initConfig error: " <> err

reinitExperimentsCache :: FFI.ProviderCacheHandle -> Logger -> IORef (Maybe (UTCTime, UTCTime)) -> TVar (Maybe Exp.ListExperimentOutput) -> TVar (Maybe ExpGrp.ListExperimentGroupsOutput) -> IO ()
reinitExperimentsCache cache logger lastModifiedRef expVar expGrpVar = do
  exps <- readTVarIO expVar
  expGrps <- readTVarIO expGrpVar
  case (exps, expGrps) of
    (Just e, Just eg) -> do
      prev <- readIORef lastModifiedRef
      let cur = (Exp.last_modified e, ExpGrp.last_modified eg)
      if prev == Just cur
        then logger $ Log.logDebug "Experiments unchanged (same last_modified), skipping reinit."
        else do
          let expsJson = toStr $ Exp.data' e
              expGrpsJson = toStr $ ExpGrp.data' eg
          result <- FFI.initExperiments cache expsJson expGrpsJson
          case result of
            Nothing -> do
              writeIORef lastModifiedRef (Just cur)
              logger $ Log.logDebug "ProviderCache experiments reinitialised."
            Just err -> logger $ Log.logError $ fromString $ "ProviderCache initExperiments error: " <> err
    _ -> pure ()

getResolvedKey ::
  (FromJSON a) =>
  SuperpositionProvider ->
  Text ->
  EvaluationContext ->
  IO (Either EvaluationError a)
getResolvedKey SuperpositionProvider {..} key ec = do
  let queryJson = toStr $ customFields ec
      tkey = T.unpack <$> targetingKey ec
  runLogger $
    when (isJust expRefreshTask && isNothing (targetingKey ec)) $
      Log.logWarn "Targeting key missing, experimentation will not have any effect."
  rcfg <- FFI.evalConfig providerCache queryJson FFI.Merge Nothing tkey
  let parser = withObject "ResolvedConfig" (.:? (fromString $ T.unpack key))
      result =
        rcfg
          >>= (eitherDecode @Object . fromString)
          >>= parseEither parser . toJSON
  pure $ case (rcfg, result) of
    (Left e, _) -> Left $ EvaluationError (General "FFI_ERROR") (Just $ "ffi: " <> fromString e)
    (_, Left e) -> Left $ EvaluationError ParseError (Just $ fromString e)
    (_, Right Nothing) -> Left $ EvaluationError FlagNotFound (Just $ "Key not found: " <> key)
    (_, Right (Just v)) -> Right v

-- | Resolve the full configuration for a given evaluation context.
resolveAllConfig ::
  SuperpositionProvider ->
  EvaluationContext ->
  IO (Either String String)
resolveAllConfig (SuperpositionProvider {..}) ec = do
  defEc <- readTVarIO _initContext
  let ec' = fromMaybe ec $ mergeEvaluationContext <$> defEc <*> Just ec
      queryJson = toStr $ customFields ec'
      tkey = T.unpack <$> targetingKey ec'
  FFI.evalConfig providerCache queryJson FFI.Merge Nothing tkey

resolveValue ::
  (FromJSON a) =>
  SuperpositionProvider ->
  Text ->
  EvaluationContext ->
  IO (EvaluationResult (ResolutionDetails a))
resolveValue p@(SuperpositionProvider {..}) key ec = do
  defEc <- readTVarIO _initContext
  let ec' = fromMaybe ec $ mergeEvaluationContext <$> defEc <*> Just ec
  result <- getResolvedKey p key ec'
  case result of
    Right v -> pure $ Right $ defaultResolution v
    Left e -> do
      let lctx = ["key" Log..= key, "error" Log..= e]
      runLogger $ Log.logError ("An error occured while resolving a key." Log.:# lctx)
      pure $ Left $ e

instance FeatureProvider SuperpositionProvider where
  getMetadata _ =  ProviderMetadata "SuperpositionProvider"
  -- TODO

  getStatus SuperpositionProvider{..} = do
    dc <- readTVarIO _initContext
    pure $ case dc of
        Just _ -> Ready
        _ -> NotReady

  initialize SuperpositionProvider {..} ec = do
    init <- isJust <$> readTVarIO _initContext
    when (not init) $ runLogger $ do
        Log.logInfo "Starting config refresh task."
        lift $ startTask configRefreshTask
        when (isJust expRefreshTask) $ Log.logInfo "Starting experiments refresh task."
        lift $ mapM_ startTask expRefreshTask
        when (isJust expGrpRefreshTask) $ Log.logInfo "Starting experiment groups refresh task."
        lift $ mapM_ startTask expGrpRefreshTask
        lift $ atomically $ writeTVar _initContext (Just ec)
    where
      startTask (DynRefreshTask t) = startRefresh t

  resolveBooleanValue = resolveValue
  resolveStringValue = resolveValue
  resolveIntegerValue = resolveValue
  resolveDoubleValue = resolveValue
  resolveObjectValue = resolveValue

-- | Close the provider, freeing the native cache.
closeSuperpositionProvider :: SuperpositionProvider -> IO ()
closeSuperpositionProvider SuperpositionProvider {..} = do
  alreadyFreed <- readIORef _cacheFreed
  when (not alreadyFreed) $ do
    writeIORef _cacheFreed True
    FFI.freeProviderCache providerCache

mkRefreshFn :: (ToJSON e) => Logger -> Text -> IO (Either e a) -> RefreshFn a
mkRefreshFn runLogger fnName call = runLogger $ do
  Log.logDebug ("Running refresh." Log.:# lctx)
  result <- lift (try @SomeException call)
  case result of
    Right (Right o) -> do
      Log.logDebug ("Refresh call okay." Log.:# lctx)
      pure (Just o)
    Right (Left e) -> do
      loge e
      pure Nothing
    Left e -> do
      loge (displayException e)
      pure Nothing
  where
    lctx = ["taskName" Log..= fnName]
    loge :: (ToJSON e) => e -> LoggingT IO ()
    loge e =
      Log.logError $
        "An error occured in a refresh call."
          Log.:# (("error" Log..= e) : lctx)

refreshConfig ::
  SuperpositionProviderOptions ->
  Logger ->
  Client.SuperpositionClient ->
  RefreshFn SDK.GetConfigOutput
refreshConfig SuperpositionProviderOptions {..} logger client =
  let builder = SDK.setOrgId orgId >> SDK.setWorkspaceId workspaceId
      call = SDK.getConfig client builder
      fnName = "ConfigRefresh"
   in mkRefreshFn logger fnName call

refreshExperiments ::
  SuperpositionProviderOptions ->
  Logger ->
  Client.SuperpositionClient ->
  RefreshFn Exp.ListExperimentOutput
refreshExperiments SuperpositionProviderOptions {..} logger client =
  let builder =
        Exp.setOrgId orgId
          >> Exp.setWorkspaceId workspaceId
          >> Exp.setStatus (Just [INPROGRESS, CREATED])
      call = Exp.listExperiment client builder
      fnName = "ExperimentsRefresh"
   in mkRefreshFn logger fnName call

refreshExperimentGroups ::
  SuperpositionProviderOptions ->
  Logger ->
  Client.SuperpositionClient ->
  RefreshFn ExpGrp.ListExperimentGroupsOutput
refreshExperimentGroups SuperpositionProviderOptions {..} logger client =
  let builder =
        ExpGrp.setOrgId orgId
          >> ExpGrp.setWorkspaceId workspaceId
          >> ExpGrp.setAll' (Just True)
      call = ExpGrp.listExperimentGroups client builder
      fnName = "ExperimentGroupsRefresh"
   in mkRefreshFn logger fnName call

newRefreshTask :: RefreshOptions -> IORef () -> RefreshFn a -> Maybe (IO ()) -> IO (DynRefreshTask a)
newRefreshTask (OnDemand ttl) _anchor rFn _onRefresh =
  DynRefreshTask . OnDemandRefresh rFn ttl
    <$> newTVarIO Nothing
newRefreshTask (Poll interval) anchor rFn onRefresh =
  PollingRefresh interval rFn
    <$> newTVarIO Nothing
    <*> pure anchor
    <*> pure onRefresh
    <&> DynRefreshTask

newSuperpositionProvider ::
  SuperpositionProviderOptions ->
  IO (Either Text SuperpositionProvider)
newSuperpositionProvider options@(SuperpositionProviderOptions {..}) = do
  manager <- Net.newTlsManager
  let result = Client.build $ do
        Client.setBearerauth (Just $ Client.BearerAuth token)
        Client.setEndpointuri endpoint
        Client.setHttpmanager manager
  case result of
    Right client -> do
      let logger :: Logger
          logger = runStdoutLoggingT . filterLogger (\_ l -> l >= logLevel)
      cache <- FFI.newProviderCache
      anchor <- newIORef ()

      -- Shared TVars for refresh callbacks to reinit the native cache
      configChan <- newTVarIO Nothing
      expChan <- newTVarIO Nothing
      expGrpChan <- newTVarIO Nothing

      -- Track last_modified to skip reinit when data hasn't changed
      configLastModified <- newIORef Nothing
      expLastModified <- newIORef Nothing

      let onConfigRefresh = reinitCache cache logger configLastModified configChan
          onExpRefresh = reinitExperimentsCache cache logger expLastModified expChan expGrpChan

      -- Config refresh: fetch, store in TVar, then reinit native cache
      let configRFn = do
            v <- refreshConfig options logger client
            mapM_ (atomically . writeTVar configChan . Just) v
            pure v
      ctask <- newRefreshTask refreshOptions anchor configRFn (Just onConfigRefresh)

      -- Experiments refresh: fetch, store in TVar, then reinit native cache
      let expRFn = do
            v <- refreshExperiments options logger client
            mapM_ (atomically . writeTVar expChan . Just) v
            pure v
      etask <-
        mapM
          (\ro -> newRefreshTask ro anchor expRFn (Just onExpRefresh))
          experimentationRefreshOptions

      -- Experiment groups refresh: fetch, store in TVar, then reinit native cache
      let expGrpRFn = do
            v <- refreshExperimentGroups options logger client
            mapM_ (atomically . writeTVar expGrpChan . Just) v
            pure v
      egtask <-
        mapM
          (\ro -> newRefreshTask ro anchor expGrpRFn (Just onExpRefresh))
          experimentationRefreshOptions

      ctxTVar <- newTVarIO Nothing
      cacheFreed <- newIORef False
      let provider =
            SuperpositionProvider
              cache
              ctask
              etask
              egtask
              ctxTVar
              anchor
              cacheFreed
              logger
              fallbackConfig
      -- Register finalizer to free cache when anchor IORef is GC'd (idempotent)
      addFinalizer anchor $ do
        alreadyFreed <- readIORef cacheFreed
        when (not alreadyFreed) $ do
          writeIORef cacheFreed True
          FFI.freeProviderCache cache
      pure $ Right provider
    Left err -> pure $ Left err

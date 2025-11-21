{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.OpenFeature.SuperpositionProvider
  ( defaultProviderOptions,
    SuperpositionProviderOptions (..),
    RefreshOptions (..),
    Log.LogLevel (..),
    newSuperpositionProvider,
    SuperpositionProvider,
  )
where

import Control.Concurrent.MVar (newEmptyMVar)
import Control.Exception (SomeException, try)
import Control.Exception.Base (displayException)
import Control.Monad (join)
import Control.Monad.Logger (LoggingT, filterLogger, runStdoutLoggingT)
import Control.Monad.Logger.Aeson qualified as Log
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON (..), encode, object, withObject, (.:?))
import Data.Aeson.Decoding (eitherDecode)
import Data.Aeson.Types (Object, parseEither)
import Data.Functor
import Data.Maybe (fromMaybe, isJust, isNothing)
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
import Io.Superposition.Model.ExperimentStatusType (ExperimentStatusType (INPROGRESS, CREATED))
import Io.Superposition.Model.GetConfigInput qualified as SDK
import Io.Superposition.Model.GetConfigOutput qualified as SDK
import Io.Superposition.Model.ListExperimentInput qualified as Exp
import Io.Superposition.Model.ListExperimentOutput qualified as Exp
import Io.Superposition.SuperpositionClient qualified as Client
import Network.HTTP.Client.TLS qualified as Net

data DynRefreshTask a = forall r. (RefreshTask r a) => DynRefreshTask (r a)

type ConfigRefreshTask = DynRefreshTask SDK.GetConfigOutput

type ExperimentsRefreshTask = DynRefreshTask Exp.ListExperimentOutput

type Logger = forall a. LoggingT IO a -> IO a

data SuperpositionProvider = SuperpositionProvider
  { configRefreshTask :: ConfigRefreshTask,
    expRefreshTask :: Maybe ExperimentsRefreshTask,
    _initContext :: TVar (Maybe EvaluationContext),
    runLogger :: Logger,
    -- TODO
    fallbackConfig :: ()
  }

mkResolveParams ::
  EvaluationContext ->
  SDK.GetConfigOutput ->
  Maybe Exp.ListExperimentOutput ->
  FFI.ResolveConfigParams
mkResolveParams ec config experiments =
  FFI.defaultResolveParams
    { FFI.defaultConfig = intoParam $ SDK.default_configs config,
      FFI.context = intoParam $ SDK.contexts config,
      FFI.overrides = intoParam $ SDK.overrides config,
      FFI.dimensionInfo = intoParam $ SDK.dimensions config,
      FFI.query = intoParam $ customFields ec,
      FFI.experimentation = toStr <$> (mkExp <$> experiments <*> targetingKey ec)
    }
  where
    toStr :: (ToJSON a) => a -> String
    toStr = LT.unpack . LTE.decodeUtf8 . encode
    intoParam :: (ToJSON a) => a -> Maybe String
    intoParam = Just . toStr
    mkExp exs tkey =
      object
        [ -- This contract is leaking. Ideally this should have
          -- been a separate arg in the FFI call.
          ("targeting_key", toJSON tkey),
          ("experiments", toJSON exs)
        ]

getResolvedKey ::
  (FromJSON a) =>
  SuperpositionProvider ->
  Text ->
  EvaluationContext ->
  IO (Either EvaluationError a)
getResolvedKey SuperpositionProvider {..} key ec = do
  config <- getTaskOutput configRefreshTask
  exs <- mapM getTaskOutput expRefreshTask
  runLogger $
    when (isJust exs && isNothing (targetingKey ec)) $
      Log.logWarn "Targeting key missing, experimentation will not have any effect."
  let params = mkResolveParams ec <$> config <*> Just (join exs)
  rcfg <- mapM FFI.getResolvedConfig params
  let parser = withObject "ResolvedConfig" (.:? (fromString $ T.unpack key))
      result =
        -- Converting to an `Either` for convienence.
        join (maybe (Left "") Right rcfg)
          >>= (eitherDecode @Object . fromString)
          >>= parseEither parser . toJSON
  pure $ case (rcfg, result) of
    -- Have to match these first, otherwise might report an error in-correclty.
    (Nothing, _) -> Left $ EvaluationError ProviderNotReady (Just "No config available to resolve.")
    (Just (Left e), _) -> Left $ EvaluationError (General "FFI_ERROR") (Just $ "ffi: " <> fromString e)
    -- Technically a parse error....
    (_, Left e) -> Left $ EvaluationError ParseError (Just $ fromString e)
    (_, Right Nothing) -> Left $ EvaluationError FlagNotFound (Just $ "Key not found: " <> key)
    (_, Right (Just v)) -> Right v
  where
    getTaskOutput (DynRefreshTask t) = getCurrent t

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
        lift $ atomically $ writeTVar _initContext (Just ec)
    where
      startTask (DynRefreshTask t) = startRefresh t

  resolveBooleanValue = resolveValue
  resolveStringValue = resolveValue
  resolveIntegerValue = resolveValue
  resolveDoubleValue = resolveValue
  resolveObjectValue = resolveValue

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

newRefreshTask :: RefreshOptions -> RefreshFn a -> IO (DynRefreshTask a)
newRefreshTask (OnDemand ttl) rFn =
  DynRefreshTask . OnDemandRefresh rFn ttl
    <$> newTVarIO Nothing
newRefreshTask (Poll interval) rFn =
  PollingRefresh interval rFn
    <$> newTVarIO Nothing
    <*> newEmptyMVar
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
      ctask <- newRefreshTask refreshOptions (refreshConfig options logger client)
      etask <-
        mapM
          (\ro -> newRefreshTask ro (refreshExperiments options logger client))
          experimentationRefreshOptions
      ctxTVar <- newTVarIO Nothing
      pure $
        Right $
          SuperpositionProvider
            ctask
            etask
            ctxTVar
            logger
            fallbackConfig
    Left err -> pure $ Left err

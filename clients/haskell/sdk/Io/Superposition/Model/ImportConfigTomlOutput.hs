module Io.Superposition.Model.ImportConfigTomlOutput (
    setStrategy,
    setDryRun,
    setConfigVersion,
    setDimensions,
    setDefaultConfigs,
    setContexts,
    build,
    ImportConfigTomlOutputBuilder,
    ImportConfigTomlOutput,
    strategy,
    dry_run,
    config_version,
    dimensions,
    default_configs,
    contexts
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ImportEntityReport
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data ImportConfigTomlOutput = ImportConfigTomlOutput {
    strategy :: Data.Text.Text,
    dry_run :: Bool,
    config_version :: Data.Maybe.Maybe Data.Text.Text,
    dimensions :: Io.Superposition.Model.ImportEntityReport.ImportEntityReport,
    default_configs :: Io.Superposition.Model.ImportEntityReport.ImportEntityReport,
    contexts :: Io.Superposition.Model.ImportEntityReport.ImportEntityReport
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ImportConfigTomlOutput where
    toJSON a = Data.Aeson.object [
        "strategy" Data.Aeson..= strategy a,
        "dry_run" Data.Aeson..= dry_run a,
        "config_version" Data.Aeson..= config_version a,
        "dimensions" Data.Aeson..= dimensions a,
        "default_configs" Data.Aeson..= default_configs a,
        "contexts" Data.Aeson..= contexts a
        ]
    

instance Io.Superposition.Utility.SerializeBody ImportConfigTomlOutput

instance Data.Aeson.FromJSON ImportConfigTomlOutput where
    parseJSON = Data.Aeson.withObject "ImportConfigTomlOutput" $ \v -> ImportConfigTomlOutput
        Data.Functor.<$> (v Data.Aeson..: "strategy")
        Control.Applicative.<*> (v Data.Aeson..: "dry_run")
        Control.Applicative.<*> (v Data.Aeson..:? "config_version")
        Control.Applicative.<*> (v Data.Aeson..: "dimensions")
        Control.Applicative.<*> (v Data.Aeson..: "default_configs")
        Control.Applicative.<*> (v Data.Aeson..: "contexts")
    



data ImportConfigTomlOutputBuilderState = ImportConfigTomlOutputBuilderState {
    strategyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dry_runBuilderState :: Data.Maybe.Maybe Bool,
    config_versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimensionsBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ImportEntityReport.ImportEntityReport,
    default_configsBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ImportEntityReport.ImportEntityReport,
    contextsBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ImportEntityReport.ImportEntityReport
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ImportConfigTomlOutputBuilderState
defaultBuilderState = ImportConfigTomlOutputBuilderState {
    strategyBuilderState = Data.Maybe.Nothing,
    dry_runBuilderState = Data.Maybe.Nothing,
    config_versionBuilderState = Data.Maybe.Nothing,
    dimensionsBuilderState = Data.Maybe.Nothing,
    default_configsBuilderState = Data.Maybe.Nothing,
    contextsBuilderState = Data.Maybe.Nothing
}

type ImportConfigTomlOutputBuilder = Control.Monad.State.Strict.State ImportConfigTomlOutputBuilderState

setStrategy :: Data.Text.Text -> ImportConfigTomlOutputBuilder ()
setStrategy value =
   Control.Monad.State.Strict.modify (\s -> (s { strategyBuilderState = Data.Maybe.Just value }))

setDryRun :: Bool -> ImportConfigTomlOutputBuilder ()
setDryRun value =
   Control.Monad.State.Strict.modify (\s -> (s { dry_runBuilderState = Data.Maybe.Just value }))

setConfigVersion :: Data.Maybe.Maybe Data.Text.Text -> ImportConfigTomlOutputBuilder ()
setConfigVersion value =
   Control.Monad.State.Strict.modify (\s -> (s { config_versionBuilderState = value }))

setDimensions :: Io.Superposition.Model.ImportEntityReport.ImportEntityReport -> ImportConfigTomlOutputBuilder ()
setDimensions value =
   Control.Monad.State.Strict.modify (\s -> (s { dimensionsBuilderState = Data.Maybe.Just value }))

setDefaultConfigs :: Io.Superposition.Model.ImportEntityReport.ImportEntityReport -> ImportConfigTomlOutputBuilder ()
setDefaultConfigs value =
   Control.Monad.State.Strict.modify (\s -> (s { default_configsBuilderState = Data.Maybe.Just value }))

setContexts :: Io.Superposition.Model.ImportEntityReport.ImportEntityReport -> ImportConfigTomlOutputBuilder ()
setContexts value =
   Control.Monad.State.Strict.modify (\s -> (s { contextsBuilderState = Data.Maybe.Just value }))

build :: ImportConfigTomlOutputBuilder () -> Data.Either.Either Data.Text.Text ImportConfigTomlOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    strategy' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigTomlOutput.ImportConfigTomlOutput.strategy is a required property.") Data.Either.Right (strategyBuilderState st)
    dry_run' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigTomlOutput.ImportConfigTomlOutput.dry_run is a required property.") Data.Either.Right (dry_runBuilderState st)
    config_version' <- Data.Either.Right (config_versionBuilderState st)
    dimensions' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigTomlOutput.ImportConfigTomlOutput.dimensions is a required property.") Data.Either.Right (dimensionsBuilderState st)
    default_configs' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigTomlOutput.ImportConfigTomlOutput.default_configs is a required property.") Data.Either.Right (default_configsBuilderState st)
    contexts' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigTomlOutput.ImportConfigTomlOutput.contexts is a required property.") Data.Either.Right (contextsBuilderState st)
    Data.Either.Right (ImportConfigTomlOutput { 
        strategy = strategy',
        dry_run = dry_run',
        config_version = config_version',
        dimensions = dimensions',
        default_configs = default_configs',
        contexts = contexts'
    })


instance Io.Superposition.Utility.FromResponseParser ImportConfigTomlOutput where
    expectedStatus = (Network.HTTP.Types.mkStatus 200 "")
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "dry_run"
        var1 <- Io.Superposition.Utility.deSerField "contexts"
        var2 <- Io.Superposition.Utility.deSerField "strategy"
        var3 <- Io.Superposition.Utility.deSerField "default_configs"
        var4 <- Io.Superposition.Utility.deSerField "config_version"
        var5 <- Io.Superposition.Utility.deSerField "dimensions"
        pure $ ImportConfigTomlOutput {
            strategy = var2,
            dry_run = var0,
            config_version = var4,
            dimensions = var5,
            default_configs = var3,
            contexts = var1
        }


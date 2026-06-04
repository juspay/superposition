module Io.Superposition.Model.ImportConfigJsonOutput (
    setMode,
    setDryRun,
    setConfigVersion,
    setDimensions,
    setDefaultConfigs,
    setContexts,
    build,
    ImportConfigJsonOutputBuilder,
    ImportConfigJsonOutput,
    mode,
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

data ImportConfigJsonOutput = ImportConfigJsonOutput {
    mode :: Data.Text.Text,
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

instance Data.Aeson.ToJSON ImportConfigJsonOutput where
    toJSON a = Data.Aeson.object [
        "mode" Data.Aeson..= mode a,
        "dry_run" Data.Aeson..= dry_run a,
        "config_version" Data.Aeson..= config_version a,
        "dimensions" Data.Aeson..= dimensions a,
        "default_configs" Data.Aeson..= default_configs a,
        "contexts" Data.Aeson..= contexts a
        ]
    

instance Io.Superposition.Utility.SerializeBody ImportConfigJsonOutput

instance Data.Aeson.FromJSON ImportConfigJsonOutput where
    parseJSON = Data.Aeson.withObject "ImportConfigJsonOutput" $ \v -> ImportConfigJsonOutput
        Data.Functor.<$> (v Data.Aeson..: "mode")
        Control.Applicative.<*> (v Data.Aeson..: "dry_run")
        Control.Applicative.<*> (v Data.Aeson..:? "config_version")
        Control.Applicative.<*> (v Data.Aeson..: "dimensions")
        Control.Applicative.<*> (v Data.Aeson..: "default_configs")
        Control.Applicative.<*> (v Data.Aeson..: "contexts")
    



data ImportConfigJsonOutputBuilderState = ImportConfigJsonOutputBuilderState {
    modeBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dry_runBuilderState :: Data.Maybe.Maybe Bool,
    config_versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimensionsBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ImportEntityReport.ImportEntityReport,
    default_configsBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ImportEntityReport.ImportEntityReport,
    contextsBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ImportEntityReport.ImportEntityReport
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ImportConfigJsonOutputBuilderState
defaultBuilderState = ImportConfigJsonOutputBuilderState {
    modeBuilderState = Data.Maybe.Nothing,
    dry_runBuilderState = Data.Maybe.Nothing,
    config_versionBuilderState = Data.Maybe.Nothing,
    dimensionsBuilderState = Data.Maybe.Nothing,
    default_configsBuilderState = Data.Maybe.Nothing,
    contextsBuilderState = Data.Maybe.Nothing
}

type ImportConfigJsonOutputBuilder = Control.Monad.State.Strict.State ImportConfigJsonOutputBuilderState

setMode :: Data.Text.Text -> ImportConfigJsonOutputBuilder ()
setMode value =
   Control.Monad.State.Strict.modify (\s -> (s { modeBuilderState = Data.Maybe.Just value }))

setDryRun :: Bool -> ImportConfigJsonOutputBuilder ()
setDryRun value =
   Control.Monad.State.Strict.modify (\s -> (s { dry_runBuilderState = Data.Maybe.Just value }))

setConfigVersion :: Data.Maybe.Maybe Data.Text.Text -> ImportConfigJsonOutputBuilder ()
setConfigVersion value =
   Control.Monad.State.Strict.modify (\s -> (s { config_versionBuilderState = value }))

setDimensions :: Io.Superposition.Model.ImportEntityReport.ImportEntityReport -> ImportConfigJsonOutputBuilder ()
setDimensions value =
   Control.Monad.State.Strict.modify (\s -> (s { dimensionsBuilderState = Data.Maybe.Just value }))

setDefaultConfigs :: Io.Superposition.Model.ImportEntityReport.ImportEntityReport -> ImportConfigJsonOutputBuilder ()
setDefaultConfigs value =
   Control.Monad.State.Strict.modify (\s -> (s { default_configsBuilderState = Data.Maybe.Just value }))

setContexts :: Io.Superposition.Model.ImportEntityReport.ImportEntityReport -> ImportConfigJsonOutputBuilder ()
setContexts value =
   Control.Monad.State.Strict.modify (\s -> (s { contextsBuilderState = Data.Maybe.Just value }))

build :: ImportConfigJsonOutputBuilder () -> Data.Either.Either Data.Text.Text ImportConfigJsonOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    mode' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigJsonOutput.ImportConfigJsonOutput.mode is a required property.") Data.Either.Right (modeBuilderState st)
    dry_run' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigJsonOutput.ImportConfigJsonOutput.dry_run is a required property.") Data.Either.Right (dry_runBuilderState st)
    config_version' <- Data.Either.Right (config_versionBuilderState st)
    dimensions' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigJsonOutput.ImportConfigJsonOutput.dimensions is a required property.") Data.Either.Right (dimensionsBuilderState st)
    default_configs' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigJsonOutput.ImportConfigJsonOutput.default_configs is a required property.") Data.Either.Right (default_configsBuilderState st)
    contexts' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigJsonOutput.ImportConfigJsonOutput.contexts is a required property.") Data.Either.Right (contextsBuilderState st)
    Data.Either.Right (ImportConfigJsonOutput { 
        mode = mode',
        dry_run = dry_run',
        config_version = config_version',
        dimensions = dimensions',
        default_configs = default_configs',
        contexts = contexts'
    })


instance Io.Superposition.Utility.FromResponseParser ImportConfigJsonOutput where
    expectedStatus = (Network.HTTP.Types.mkStatus 200 "")
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "mode"
        var1 <- Io.Superposition.Utility.deSerField "dry_run"
        var2 <- Io.Superposition.Utility.deSerField "contexts"
        var3 <- Io.Superposition.Utility.deSerField "default_configs"
        var4 <- Io.Superposition.Utility.deSerField "config_version"
        var5 <- Io.Superposition.Utility.deSerField "dimensions"
        pure $ ImportConfigJsonOutput {
            mode = var0,
            dry_run = var1,
            config_version = var4,
            dimensions = var5,
            default_configs = var3,
            contexts = var2
        }


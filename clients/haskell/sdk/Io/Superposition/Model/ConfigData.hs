module Io.Superposition.Model.ConfigData (
    setContexts,
    setOverrides,
    setDefaultConfigs,
    setDimensions,
    build,
    ConfigDataBuilder,
    ConfigData,
    contexts,
    overrides,
    default_configs,
    dimensions
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ContextPartial
import qualified Io.Superposition.Model.DimensionInfo
import qualified Io.Superposition.Utility

data ConfigData = ConfigData {
    contexts :: [] Io.Superposition.Model.ContextPartial.ContextPartial,
    overrides :: Data.Map.Map Data.Text.Text (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    default_configs :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    dimensions :: Data.Map.Map Data.Text.Text Io.Superposition.Model.DimensionInfo.DimensionInfo
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ConfigData where
    toJSON a = Data.Aeson.object [
        "contexts" Data.Aeson..= contexts a,
        "overrides" Data.Aeson..= overrides a,
        "default_configs" Data.Aeson..= default_configs a,
        "dimensions" Data.Aeson..= dimensions a
        ]
    

instance Io.Superposition.Utility.SerializeBody ConfigData

instance Data.Aeson.FromJSON ConfigData where
    parseJSON = Data.Aeson.withObject "ConfigData" $ \v -> ConfigData
        Data.Functor.<$> (v Data.Aeson..: "contexts")
        Control.Applicative.<*> (v Data.Aeson..: "overrides")
        Control.Applicative.<*> (v Data.Aeson..: "default_configs")
        Control.Applicative.<*> (v Data.Aeson..: "dimensions")
    



data ConfigDataBuilderState = ConfigDataBuilderState {
    contextsBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextPartial.ContextPartial),
    overridesBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text (Data.Map.Map Data.Text.Text Data.Aeson.Value)),
    default_configsBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    dimensionsBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Io.Superposition.Model.DimensionInfo.DimensionInfo)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ConfigDataBuilderState
defaultBuilderState = ConfigDataBuilderState {
    contextsBuilderState = Data.Maybe.Nothing,
    overridesBuilderState = Data.Maybe.Nothing,
    default_configsBuilderState = Data.Maybe.Nothing,
    dimensionsBuilderState = Data.Maybe.Nothing
}

type ConfigDataBuilder = Control.Monad.State.Strict.State ConfigDataBuilderState

setContexts :: [] Io.Superposition.Model.ContextPartial.ContextPartial -> ConfigDataBuilder ()
setContexts value =
   Control.Monad.State.Strict.modify (\s -> (s { contextsBuilderState = Data.Maybe.Just value }))

setOverrides :: Data.Map.Map Data.Text.Text (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> ConfigDataBuilder ()
setOverrides value =
   Control.Monad.State.Strict.modify (\s -> (s { overridesBuilderState = Data.Maybe.Just value }))

setDefaultConfigs :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> ConfigDataBuilder ()
setDefaultConfigs value =
   Control.Monad.State.Strict.modify (\s -> (s { default_configsBuilderState = Data.Maybe.Just value }))

setDimensions :: Data.Map.Map Data.Text.Text Io.Superposition.Model.DimensionInfo.DimensionInfo -> ConfigDataBuilder ()
setDimensions value =
   Control.Monad.State.Strict.modify (\s -> (s { dimensionsBuilderState = Data.Maybe.Just value }))

build :: ConfigDataBuilder () -> Data.Either.Either Data.Text.Text ConfigData
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    contexts' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ConfigData.ConfigData.contexts is a required property.") Data.Either.Right (contextsBuilderState st)
    overrides' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ConfigData.ConfigData.overrides is a required property.") Data.Either.Right (overridesBuilderState st)
    default_configs' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ConfigData.ConfigData.default_configs is a required property.") Data.Either.Right (default_configsBuilderState st)
    dimensions' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ConfigData.ConfigData.dimensions is a required property.") Data.Either.Right (dimensionsBuilderState st)
    Data.Either.Right (ConfigData { 
        contexts = contexts',
        overrides = overrides',
        default_configs = default_configs',
        dimensions = dimensions'
    })



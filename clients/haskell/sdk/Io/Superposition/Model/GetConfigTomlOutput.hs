module Io.Superposition.Model.GetConfigTomlOutput (
    setTomlConfig,
    setLastModified,
    build,
    GetConfigTomlOutputBuilder,
    GetConfigTomlOutput,
    toml_config,
    last_modified
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data GetConfigTomlOutput = GetConfigTomlOutput {
    toml_config :: Data.Text.Text,
    last_modified :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetConfigTomlOutput where
    toJSON a = Data.Aeson.object [
        "toml_config" Data.Aeson..= toml_config a,
        "last_modified" Data.Aeson..= last_modified a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetConfigTomlOutput

instance Data.Aeson.FromJSON GetConfigTomlOutput where
    parseJSON = Data.Aeson.withObject "GetConfigTomlOutput" $ \v -> GetConfigTomlOutput
        Data.Functor.<$> (v Data.Aeson..: "toml_config")
        Control.Applicative.<*> (v Data.Aeson..:? "last_modified")
    



data GetConfigTomlOutputBuilderState = GetConfigTomlOutputBuilderState {
    toml_configBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetConfigTomlOutputBuilderState
defaultBuilderState = GetConfigTomlOutputBuilderState {
    toml_configBuilderState = Data.Maybe.Nothing,
    last_modifiedBuilderState = Data.Maybe.Nothing
}

type GetConfigTomlOutputBuilder = Control.Monad.State.Strict.State GetConfigTomlOutputBuilderState

setTomlConfig :: Data.Text.Text -> GetConfigTomlOutputBuilder ()
setTomlConfig value =
   Control.Monad.State.Strict.modify (\s -> (s { toml_configBuilderState = Data.Maybe.Just value }))

setLastModified :: Data.Maybe.Maybe Data.Time.UTCTime -> GetConfigTomlOutputBuilder ()
setLastModified value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modifiedBuilderState = value }))

build :: GetConfigTomlOutputBuilder () -> Data.Either.Either Data.Text.Text GetConfigTomlOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    toml_config' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetConfigTomlOutput.GetConfigTomlOutput.toml_config is a required property.") Data.Either.Right (toml_configBuilderState st)
    last_modified' <- Data.Either.Right (last_modifiedBuilderState st)
    Data.Either.Right (GetConfigTomlOutput { 
        toml_config = toml_config',
        last_modified = last_modified'
    })


instance Io.Superposition.Utility.FromResponseParser GetConfigTomlOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        var0 <- Io.Superposition.Utility.deSerHeader "last-modified"
        var1 <- Io.Superposition.Utility.deSerBody
        pure $ GetConfigTomlOutput {
            toml_config = var1,
            last_modified = var0
        }


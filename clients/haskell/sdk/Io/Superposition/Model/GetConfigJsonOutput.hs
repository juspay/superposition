module Io.Superposition.Model.GetConfigJsonOutput (
    setJsonConfig,
    setLastModified,
    build,
    GetConfigJsonOutputBuilder,
    GetConfigJsonOutput,
    json_config,
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

data GetConfigJsonOutput = GetConfigJsonOutput {
    json_config :: Data.Text.Text,
    last_modified :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetConfigJsonOutput where
    toJSON a = Data.Aeson.object [
        "json_config" Data.Aeson..= json_config a,
        "last_modified" Data.Aeson..= last_modified a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetConfigJsonOutput

instance Data.Aeson.FromJSON GetConfigJsonOutput where
    parseJSON = Data.Aeson.withObject "GetConfigJsonOutput" $ \v -> GetConfigJsonOutput
        Data.Functor.<$> (v Data.Aeson..: "json_config")
        Control.Applicative.<*> (v Data.Aeson..:? "last_modified")
    



data GetConfigJsonOutputBuilderState = GetConfigJsonOutputBuilderState {
    json_configBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetConfigJsonOutputBuilderState
defaultBuilderState = GetConfigJsonOutputBuilderState {
    json_configBuilderState = Data.Maybe.Nothing,
    last_modifiedBuilderState = Data.Maybe.Nothing
}

type GetConfigJsonOutputBuilder = Control.Monad.State.Strict.State GetConfigJsonOutputBuilderState

setJsonConfig :: Data.Text.Text -> GetConfigJsonOutputBuilder ()
setJsonConfig value =
   Control.Monad.State.Strict.modify (\s -> (s { json_configBuilderState = Data.Maybe.Just value }))

setLastModified :: Data.Maybe.Maybe Data.Time.UTCTime -> GetConfigJsonOutputBuilder ()
setLastModified value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modifiedBuilderState = value }))

build :: GetConfigJsonOutputBuilder () -> Data.Either.Either Data.Text.Text GetConfigJsonOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    json_config' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetConfigJsonOutput.GetConfigJsonOutput.json_config is a required property.") Data.Either.Right (json_configBuilderState st)
    last_modified' <- Data.Either.Right (last_modifiedBuilderState st)
    Data.Either.Right (GetConfigJsonOutput { 
        json_config = json_config',
        last_modified = last_modified'
    })


instance Io.Superposition.Utility.FromResponseParser GetConfigJsonOutput where
    expectedStatus = (Network.HTTP.Types.mkStatus 200 "")
    responseParser = do
        var0 <- Io.Superposition.Utility.deSerHeader "last-modified"
        var1 <- Io.Superposition.Utility.deSerBody
        pure $ GetConfigJsonOutput {
            json_config = var1,
            last_modified = var0
        }


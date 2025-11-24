module Io.Superposition.Model.GetVersionOutput (
    setId',
    setConfig,
    setConfigHash,
    setCreatedAt,
    setDescription,
    setTags,
    build,
    GetVersionOutputBuilder,
    GetVersionOutput,
    id',
    config,
    config_hash,
    created_at,
    description,
    tags
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

data GetVersionOutput = GetVersionOutput {
    id' :: Data.Text.Text,
    config :: Data.Aeson.Value,
    config_hash :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    description :: Data.Text.Text,
    tags :: Data.Maybe.Maybe ([] Data.Text.Text)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetVersionOutput where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "config" Data.Aeson..= config a,
        "config_hash" Data.Aeson..= config_hash a,
        "created_at" Data.Aeson..= created_at a,
        "description" Data.Aeson..= description a,
        "tags" Data.Aeson..= tags a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetVersionOutput

instance Data.Aeson.FromJSON GetVersionOutput where
    parseJSON = Data.Aeson.withObject "GetVersionOutput" $ \v -> GetVersionOutput
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "config")
        Control.Applicative.<*> (v Data.Aeson..: "config_hash")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..:? "tags")
    



data GetVersionOutputBuilderState = GetVersionOutputBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    configBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    config_hashBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    tagsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetVersionOutputBuilderState
defaultBuilderState = GetVersionOutputBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    configBuilderState = Data.Maybe.Nothing,
    config_hashBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    tagsBuilderState = Data.Maybe.Nothing
}

type GetVersionOutputBuilder = Control.Monad.State.Strict.State GetVersionOutputBuilderState

setId' :: Data.Text.Text -> GetVersionOutputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setConfig :: Data.Aeson.Value -> GetVersionOutputBuilder ()
setConfig value =
   Control.Monad.State.Strict.modify (\s -> (s { configBuilderState = Data.Maybe.Just value }))

setConfigHash :: Data.Text.Text -> GetVersionOutputBuilder ()
setConfigHash value =
   Control.Monad.State.Strict.modify (\s -> (s { config_hashBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> GetVersionOutputBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> GetVersionOutputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setTags :: Data.Maybe.Maybe ([] Data.Text.Text) -> GetVersionOutputBuilder ()
setTags value =
   Control.Monad.State.Strict.modify (\s -> (s { tagsBuilderState = value }))

build :: GetVersionOutputBuilder () -> Data.Either.Either Data.Text.Text GetVersionOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetVersionOutput.GetVersionOutput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    config' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetVersionOutput.GetVersionOutput.config is a required property.") Data.Either.Right (configBuilderState st)
    config_hash' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetVersionOutput.GetVersionOutput.config_hash is a required property.") Data.Either.Right (config_hashBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetVersionOutput.GetVersionOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetVersionOutput.GetVersionOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    tags' <- Data.Either.Right (tagsBuilderState st)
    Data.Either.Right (GetVersionOutput { 
        id' = id'',
        config = config',
        config_hash = config_hash',
        created_at = created_at',
        description = description',
        tags = tags'
    })


instance Io.Superposition.Utility.FromResponseParser GetVersionOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "created_at"
        var1 <- Io.Superposition.Utility.deSerField "description"
        var2 <- Io.Superposition.Utility.deSerField "id"
        var3 <- Io.Superposition.Utility.deSerField "config"
        var4 <- Io.Superposition.Utility.deSerField "config_hash"
        var5 <- Io.Superposition.Utility.deSerField "tags"
        pure $ GetVersionOutput {
            id' = var2,
            config = var3,
            config_hash = var4,
            created_at = var0,
            description = var1,
            tags = var5
        }


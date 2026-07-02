module Io.Superposition.Model.ImportConfigJsonInput (
    setWorkspaceId,
    setOrgId,
    setStrategy,
    setOnError,
    setDryRun,
    setConfigTags,
    setJsonConfig,
    build,
    ImportConfigJsonInputBuilder,
    ImportConfigJsonInput,
    workspace_id,
    org_id,
    strategy,
    on_error,
    dry_run,
    config_tags,
    json_config
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
import qualified Io.Superposition.Model.ImportOnError
import qualified Io.Superposition.Model.ImportStrategy
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ImportConfigJsonInput = ImportConfigJsonInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    strategy :: Data.Maybe.Maybe Io.Superposition.Model.ImportStrategy.ImportStrategy,
    on_error :: Data.Maybe.Maybe Io.Superposition.Model.ImportOnError.ImportOnError,
    dry_run :: Data.Maybe.Maybe Bool,
    config_tags :: Data.Maybe.Maybe Data.Text.Text,
    json_config :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ImportConfigJsonInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "strategy" Data.Aeson..= strategy a,
        "on_error" Data.Aeson..= on_error a,
        "dry_run" Data.Aeson..= dry_run a,
        "config_tags" Data.Aeson..= config_tags a,
        "json_config" Data.Aeson..= json_config a
        ]
    

instance Io.Superposition.Utility.SerializeBody ImportConfigJsonInput

instance Data.Aeson.FromJSON ImportConfigJsonInput where
    parseJSON = Data.Aeson.withObject "ImportConfigJsonInput" $ \v -> ImportConfigJsonInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "strategy")
        Control.Applicative.<*> (v Data.Aeson..:? "on_error")
        Control.Applicative.<*> (v Data.Aeson..:? "dry_run")
        Control.Applicative.<*> (v Data.Aeson..:? "config_tags")
        Control.Applicative.<*> (v Data.Aeson..: "json_config")
    



data ImportConfigJsonInputBuilderState = ImportConfigJsonInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ImportStrategy.ImportStrategy,
    on_errorBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ImportOnError.ImportOnError,
    dry_runBuilderState :: Data.Maybe.Maybe Bool,
    config_tagsBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    json_configBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ImportConfigJsonInputBuilderState
defaultBuilderState = ImportConfigJsonInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    strategyBuilderState = Data.Maybe.Nothing,
    on_errorBuilderState = Data.Maybe.Nothing,
    dry_runBuilderState = Data.Maybe.Nothing,
    config_tagsBuilderState = Data.Maybe.Nothing,
    json_configBuilderState = Data.Maybe.Nothing
}

type ImportConfigJsonInputBuilder = Control.Monad.State.Strict.State ImportConfigJsonInputBuilderState

setWorkspaceId :: Data.Text.Text -> ImportConfigJsonInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ImportConfigJsonInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setStrategy :: Data.Maybe.Maybe Io.Superposition.Model.ImportStrategy.ImportStrategy -> ImportConfigJsonInputBuilder ()
setStrategy value =
   Control.Monad.State.Strict.modify (\s -> (s { strategyBuilderState = value }))

setOnError :: Data.Maybe.Maybe Io.Superposition.Model.ImportOnError.ImportOnError -> ImportConfigJsonInputBuilder ()
setOnError value =
   Control.Monad.State.Strict.modify (\s -> (s { on_errorBuilderState = value }))

setDryRun :: Data.Maybe.Maybe Bool -> ImportConfigJsonInputBuilder ()
setDryRun value =
   Control.Monad.State.Strict.modify (\s -> (s { dry_runBuilderState = value }))

setConfigTags :: Data.Maybe.Maybe Data.Text.Text -> ImportConfigJsonInputBuilder ()
setConfigTags value =
   Control.Monad.State.Strict.modify (\s -> (s { config_tagsBuilderState = value }))

setJsonConfig :: Data.Text.Text -> ImportConfigJsonInputBuilder ()
setJsonConfig value =
   Control.Monad.State.Strict.modify (\s -> (s { json_configBuilderState = Data.Maybe.Just value }))

build :: ImportConfigJsonInputBuilder () -> Data.Either.Either Data.Text.Text ImportConfigJsonInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigJsonInput.ImportConfigJsonInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigJsonInput.ImportConfigJsonInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    strategy' <- Data.Either.Right (strategyBuilderState st)
    on_error' <- Data.Either.Right (on_errorBuilderState st)
    dry_run' <- Data.Either.Right (dry_runBuilderState st)
    config_tags' <- Data.Either.Right (config_tagsBuilderState st)
    json_config' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportConfigJsonInput.ImportConfigJsonInput.json_config is a required property.") Data.Either.Right (json_configBuilderState st)
    Data.Either.Right (ImportConfigJsonInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        strategy = strategy',
        on_error = on_error',
        dry_run = dry_run',
        config_tags = config_tags',
        json_config = json_config'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ImportConfigJsonInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "config",
            "json",
            "import"
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serHeader "x-import-on-error" (on_error self)
        Io.Superposition.Utility.serHeader "x-import-dry-run" (dry_run self)
        Io.Superposition.Utility.serHeader "x-import-strategy" (strategy self)
        Io.Superposition.Utility.serHeader "x-config-tags" (config_tags self)
        Io.Superposition.Utility.serBody "text/plain" (json_config self)


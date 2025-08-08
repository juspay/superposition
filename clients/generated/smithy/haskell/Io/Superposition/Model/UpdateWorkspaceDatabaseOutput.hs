module Io.Superposition.Model.UpdateWorkspaceDatabaseOutput (
    setWorkspaceName,
    setOrganisationId,
    setOrganisationName,
    setWorkspaceSchemaName,
    setWorkspaceStatus,
    setWorkspaceAdminEmail,
    setConfigVersion,
    setCreatedBy,
    setLastModifiedBy,
    setLastModifiedAt,
    setCreatedAt,
    setMandatoryDimensions,
    setStrictMode,
    setMetrics,
    setAllowExperimentSelfApproval,
    setAutoPopulateControl,
    build,
    UpdateWorkspaceDatabaseOutputBuilder,
    UpdateWorkspaceDatabaseOutput,
    workspace_name,
    organisation_id,
    organisation_name,
    workspace_schema_name,
    workspace_status,
    workspace_admin_email,
    config_version,
    created_by,
    last_modified_by,
    last_modified_at,
    created_at,
    mandatory_dimensions,
    strict_mode,
    metrics,
    allow_experiment_self_approval,
    auto_populate_control
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.WorkspaceStatus

data UpdateWorkspaceDatabaseOutput = UpdateWorkspaceDatabaseOutput {
    workspace_name :: Data.Text.Text,
    organisation_id :: Data.Text.Text,
    organisation_name :: Data.Text.Text,
    workspace_schema_name :: Data.Text.Text,
    workspace_status :: Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus,
    workspace_admin_email :: Data.Text.Text,
    config_version :: Data.Maybe.Maybe Data.Text.Text,
    created_by :: Data.Text.Text,
    last_modified_by :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime,
    created_at :: Data.Time.UTCTime,
    mandatory_dimensions :: Data.Maybe.Maybe ([] Data.Text.Text),
    strict_mode :: Bool,
    metrics :: Data.Maybe.Maybe Data.Aeson.Value,
    allow_experiment_self_approval :: Bool,
    auto_populate_control :: Bool
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateWorkspaceDatabaseOutput where
    toJSON a = Data.Aeson.object [
        "workspace_name" Data.Aeson..= workspace_name a,
        "organisation_id" Data.Aeson..= organisation_id a,
        "organisation_name" Data.Aeson..= organisation_name a,
        "workspace_schema_name" Data.Aeson..= workspace_schema_name a,
        "workspace_status" Data.Aeson..= workspace_status a,
        "workspace_admin_email" Data.Aeson..= workspace_admin_email a,
        "config_version" Data.Aeson..= config_version a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "last_modified_at" Data.Aeson..= last_modified_at a,
        "created_at" Data.Aeson..= created_at a,
        "mandatory_dimensions" Data.Aeson..= mandatory_dimensions a,
        "strict_mode" Data.Aeson..= strict_mode a,
        "metrics" Data.Aeson..= metrics a,
        "allow_experiment_self_approval" Data.Aeson..= allow_experiment_self_approval a,
        "auto_populate_control" Data.Aeson..= auto_populate_control a
        ]
    


instance Data.Aeson.FromJSON UpdateWorkspaceDatabaseOutput where
    parseJSON = Data.Aeson.withObject "UpdateWorkspaceDatabaseOutput" $ \v -> UpdateWorkspaceDatabaseOutput
        Data.Functor.<$> (v Data.Aeson..: "workspace_name")
        Control.Applicative.<*> (v Data.Aeson..: "organisation_id")
        Control.Applicative.<*> (v Data.Aeson..: "organisation_name")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_schema_name")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_status")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_admin_email")
        Control.Applicative.<*> (v Data.Aeson..: "config_version")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "mandatory_dimensions")
        Control.Applicative.<*> (v Data.Aeson..: "strict_mode")
        Control.Applicative.<*> (v Data.Aeson..: "metrics")
        Control.Applicative.<*> (v Data.Aeson..: "allow_experiment_self_approval")
        Control.Applicative.<*> (v Data.Aeson..: "auto_populate_control")
    



data UpdateWorkspaceDatabaseOutputBuilderState = UpdateWorkspaceDatabaseOutputBuilderState {
    workspace_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    organisation_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    organisation_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_schema_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus,
    workspace_admin_emailBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    config_versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    mandatory_dimensionsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    strict_modeBuilderState :: Data.Maybe.Maybe Bool,
    metricsBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    allow_experiment_self_approvalBuilderState :: Data.Maybe.Maybe Bool,
    auto_populate_controlBuilderState :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateWorkspaceDatabaseOutputBuilderState
defaultBuilderState = UpdateWorkspaceDatabaseOutputBuilderState {
    workspace_nameBuilderState = Data.Maybe.Nothing,
    organisation_idBuilderState = Data.Maybe.Nothing,
    organisation_nameBuilderState = Data.Maybe.Nothing,
    workspace_schema_nameBuilderState = Data.Maybe.Nothing,
    workspace_statusBuilderState = Data.Maybe.Nothing,
    workspace_admin_emailBuilderState = Data.Maybe.Nothing,
    config_versionBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    mandatory_dimensionsBuilderState = Data.Maybe.Nothing,
    strict_modeBuilderState = Data.Maybe.Nothing,
    metricsBuilderState = Data.Maybe.Nothing,
    allow_experiment_self_approvalBuilderState = Data.Maybe.Nothing,
    auto_populate_controlBuilderState = Data.Maybe.Nothing
}

newtype UpdateWorkspaceDatabaseOutputBuilder a = UpdateWorkspaceDatabaseOutputBuilder {
    runUpdateWorkspaceDatabaseOutputBuilder :: UpdateWorkspaceDatabaseOutputBuilderState -> (UpdateWorkspaceDatabaseOutputBuilderState, a)
}

instance Data.Functor.Functor UpdateWorkspaceDatabaseOutputBuilder where
    fmap f (UpdateWorkspaceDatabaseOutputBuilder g) =
        UpdateWorkspaceDatabaseOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateWorkspaceDatabaseOutputBuilder where
    pure a = UpdateWorkspaceDatabaseOutputBuilder (\s -> (s, a))
    (UpdateWorkspaceDatabaseOutputBuilder f) <*> (UpdateWorkspaceDatabaseOutputBuilder g) = UpdateWorkspaceDatabaseOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateWorkspaceDatabaseOutputBuilder where
    (UpdateWorkspaceDatabaseOutputBuilder f) >>= g = UpdateWorkspaceDatabaseOutputBuilder (\s ->
        let (s', a) = f s
            (UpdateWorkspaceDatabaseOutputBuilder h) = g a
        in h s')

setWorkspaceName :: Data.Text.Text -> UpdateWorkspaceDatabaseOutputBuilder ()
setWorkspaceName value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { workspace_nameBuilderState = Data.Maybe.Just value }, ()))

setOrganisationId :: Data.Text.Text -> UpdateWorkspaceDatabaseOutputBuilder ()
setOrganisationId value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { organisation_idBuilderState = Data.Maybe.Just value }, ()))

setOrganisationName :: Data.Text.Text -> UpdateWorkspaceDatabaseOutputBuilder ()
setOrganisationName value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { organisation_nameBuilderState = Data.Maybe.Just value }, ()))

setWorkspaceSchemaName :: Data.Text.Text -> UpdateWorkspaceDatabaseOutputBuilder ()
setWorkspaceSchemaName value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { workspace_schema_nameBuilderState = Data.Maybe.Just value }, ()))

setWorkspaceStatus :: Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus -> UpdateWorkspaceDatabaseOutputBuilder ()
setWorkspaceStatus value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { workspace_statusBuilderState = Data.Maybe.Just value }, ()))

setWorkspaceAdminEmail :: Data.Text.Text -> UpdateWorkspaceDatabaseOutputBuilder ()
setWorkspaceAdminEmail value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { workspace_admin_emailBuilderState = Data.Maybe.Just value }, ()))

setConfigVersion :: Data.Maybe.Maybe Data.Text.Text -> UpdateWorkspaceDatabaseOutputBuilder ()
setConfigVersion value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { config_versionBuilderState = value }, ()))

setCreatedBy :: Data.Text.Text -> UpdateWorkspaceDatabaseOutputBuilder ()
setCreatedBy value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> UpdateWorkspaceDatabaseOutputBuilder ()
setLastModifiedBy value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedAt :: Data.Time.UTCTime -> UpdateWorkspaceDatabaseOutputBuilder ()
setLastModifiedAt value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }, ()))

setCreatedAt :: Data.Time.UTCTime -> UpdateWorkspaceDatabaseOutputBuilder ()
setCreatedAt value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setMandatoryDimensions :: Data.Maybe.Maybe ([] Data.Text.Text) -> UpdateWorkspaceDatabaseOutputBuilder ()
setMandatoryDimensions value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { mandatory_dimensionsBuilderState = value }, ()))

setStrictMode :: Bool -> UpdateWorkspaceDatabaseOutputBuilder ()
setStrictMode value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { strict_modeBuilderState = Data.Maybe.Just value }, ()))

setMetrics :: Data.Maybe.Maybe Data.Aeson.Value -> UpdateWorkspaceDatabaseOutputBuilder ()
setMetrics value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { metricsBuilderState = value }, ()))

setAllowExperimentSelfApproval :: Bool -> UpdateWorkspaceDatabaseOutputBuilder ()
setAllowExperimentSelfApproval value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { allow_experiment_self_approvalBuilderState = Data.Maybe.Just value }, ()))

setAutoPopulateControl :: Bool -> UpdateWorkspaceDatabaseOutputBuilder ()
setAutoPopulateControl value =
   UpdateWorkspaceDatabaseOutputBuilder (\s -> (s { auto_populate_controlBuilderState = Data.Maybe.Just value }, ()))

build :: UpdateWorkspaceDatabaseOutputBuilder () -> Data.Either.Either Data.Text.Text UpdateWorkspaceDatabaseOutput
build builder = do
    let (st, _) = runUpdateWorkspaceDatabaseOutputBuilder builder defaultBuilderState
    workspace_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.workspace_name is a required property.") Data.Either.Right (workspace_nameBuilderState st)
    organisation_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.organisation_id is a required property.") Data.Either.Right (organisation_idBuilderState st)
    organisation_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.organisation_name is a required property.") Data.Either.Right (organisation_nameBuilderState st)
    workspace_schema_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.workspace_schema_name is a required property.") Data.Either.Right (workspace_schema_nameBuilderState st)
    workspace_status' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.workspace_status is a required property.") Data.Either.Right (workspace_statusBuilderState st)
    workspace_admin_email' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.workspace_admin_email is a required property.") Data.Either.Right (workspace_admin_emailBuilderState st)
    config_version' <- Data.Either.Right (config_versionBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    mandatory_dimensions' <- Data.Either.Right (mandatory_dimensionsBuilderState st)
    strict_mode' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.strict_mode is a required property.") Data.Either.Right (strict_modeBuilderState st)
    metrics' <- Data.Either.Right (metricsBuilderState st)
    allow_experiment_self_approval' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.allow_experiment_self_approval is a required property.") Data.Either.Right (allow_experiment_self_approvalBuilderState st)
    auto_populate_control' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput.auto_populate_control is a required property.") Data.Either.Right (auto_populate_controlBuilderState st)
    Data.Either.Right (UpdateWorkspaceDatabaseOutput { 
        workspace_name = workspace_name',
        organisation_id = organisation_id',
        organisation_name = organisation_name',
        workspace_schema_name = workspace_schema_name',
        workspace_status = workspace_status',
        workspace_admin_email = workspace_admin_email',
        config_version = config_version',
        created_by = created_by',
        last_modified_by = last_modified_by',
        last_modified_at = last_modified_at',
        created_at = created_at',
        mandatory_dimensions = mandatory_dimensions',
        strict_mode = strict_mode',
        metrics = metrics',
        allow_experiment_self_approval = allow_experiment_self_approval',
        auto_populate_control = auto_populate_control'
    })



module Io.Superposition.Model.GetWorkspaceOutput (
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
    setMetrics,
    setAllowExperimentSelfApproval,
    setAutoPopulateControl,
    setEnableContextValidation,
    setEnableChangeReasonValidation,
    build,
    GetWorkspaceOutputBuilder,
    GetWorkspaceOutput,
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
    metrics,
    allow_experiment_self_approval,
    auto_populate_control,
    enable_context_validation,
    enable_change_reason_validation
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
import qualified Io.Superposition.Model.WorkspaceStatus
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data GetWorkspaceOutput = GetWorkspaceOutput {
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
    metrics :: Data.Aeson.Value,
    allow_experiment_self_approval :: Bool,
    auto_populate_control :: Bool,
    enable_context_validation :: Bool,
    enable_change_reason_validation :: Bool
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetWorkspaceOutput where
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
        "metrics" Data.Aeson..= metrics a,
        "allow_experiment_self_approval" Data.Aeson..= allow_experiment_self_approval a,
        "auto_populate_control" Data.Aeson..= auto_populate_control a,
        "enable_context_validation" Data.Aeson..= enable_context_validation a,
        "enable_change_reason_validation" Data.Aeson..= enable_change_reason_validation a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetWorkspaceOutput

instance Data.Aeson.FromJSON GetWorkspaceOutput where
    parseJSON = Data.Aeson.withObject "GetWorkspaceOutput" $ \v -> GetWorkspaceOutput
        Data.Functor.<$> (v Data.Aeson..: "workspace_name")
        Control.Applicative.<*> (v Data.Aeson..: "organisation_id")
        Control.Applicative.<*> (v Data.Aeson..: "organisation_name")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_schema_name")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_status")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_admin_email")
        Control.Applicative.<*> (v Data.Aeson..:? "config_version")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..:? "mandatory_dimensions")
        Control.Applicative.<*> (v Data.Aeson..: "metrics")
        Control.Applicative.<*> (v Data.Aeson..: "allow_experiment_self_approval")
        Control.Applicative.<*> (v Data.Aeson..: "auto_populate_control")
        Control.Applicative.<*> (v Data.Aeson..: "enable_context_validation")
        Control.Applicative.<*> (v Data.Aeson..: "enable_change_reason_validation")
    



data GetWorkspaceOutputBuilderState = GetWorkspaceOutputBuilderState {
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
    metricsBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    allow_experiment_self_approvalBuilderState :: Data.Maybe.Maybe Bool,
    auto_populate_controlBuilderState :: Data.Maybe.Maybe Bool,
    enable_context_validationBuilderState :: Data.Maybe.Maybe Bool,
    enable_change_reason_validationBuilderState :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetWorkspaceOutputBuilderState
defaultBuilderState = GetWorkspaceOutputBuilderState {
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
    metricsBuilderState = Data.Maybe.Nothing,
    allow_experiment_self_approvalBuilderState = Data.Maybe.Nothing,
    auto_populate_controlBuilderState = Data.Maybe.Nothing,
    enable_context_validationBuilderState = Data.Maybe.Nothing,
    enable_change_reason_validationBuilderState = Data.Maybe.Nothing
}

type GetWorkspaceOutputBuilder = Control.Monad.State.Strict.State GetWorkspaceOutputBuilderState

setWorkspaceName :: Data.Text.Text -> GetWorkspaceOutputBuilder ()
setWorkspaceName value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_nameBuilderState = Data.Maybe.Just value }))

setOrganisationId :: Data.Text.Text -> GetWorkspaceOutputBuilder ()
setOrganisationId value =
   Control.Monad.State.Strict.modify (\s -> (s { organisation_idBuilderState = Data.Maybe.Just value }))

setOrganisationName :: Data.Text.Text -> GetWorkspaceOutputBuilder ()
setOrganisationName value =
   Control.Monad.State.Strict.modify (\s -> (s { organisation_nameBuilderState = Data.Maybe.Just value }))

setWorkspaceSchemaName :: Data.Text.Text -> GetWorkspaceOutputBuilder ()
setWorkspaceSchemaName value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_schema_nameBuilderState = Data.Maybe.Just value }))

setWorkspaceStatus :: Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus -> GetWorkspaceOutputBuilder ()
setWorkspaceStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_statusBuilderState = Data.Maybe.Just value }))

setWorkspaceAdminEmail :: Data.Text.Text -> GetWorkspaceOutputBuilder ()
setWorkspaceAdminEmail value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_admin_emailBuilderState = Data.Maybe.Just value }))

setConfigVersion :: Data.Maybe.Maybe Data.Text.Text -> GetWorkspaceOutputBuilder ()
setConfigVersion value =
   Control.Monad.State.Strict.modify (\s -> (s { config_versionBuilderState = value }))

setCreatedBy :: Data.Text.Text -> GetWorkspaceOutputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> GetWorkspaceOutputBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> GetWorkspaceOutputBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> GetWorkspaceOutputBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setMandatoryDimensions :: Data.Maybe.Maybe ([] Data.Text.Text) -> GetWorkspaceOutputBuilder ()
setMandatoryDimensions value =
   Control.Monad.State.Strict.modify (\s -> (s { mandatory_dimensionsBuilderState = value }))

setMetrics :: Data.Aeson.Value -> GetWorkspaceOutputBuilder ()
setMetrics value =
   Control.Monad.State.Strict.modify (\s -> (s { metricsBuilderState = Data.Maybe.Just value }))

setAllowExperimentSelfApproval :: Bool -> GetWorkspaceOutputBuilder ()
setAllowExperimentSelfApproval value =
   Control.Monad.State.Strict.modify (\s -> (s { allow_experiment_self_approvalBuilderState = Data.Maybe.Just value }))

setAutoPopulateControl :: Bool -> GetWorkspaceOutputBuilder ()
setAutoPopulateControl value =
   Control.Monad.State.Strict.modify (\s -> (s { auto_populate_controlBuilderState = Data.Maybe.Just value }))

setEnableContextValidation :: Bool -> GetWorkspaceOutputBuilder ()
setEnableContextValidation value =
   Control.Monad.State.Strict.modify (\s -> (s { enable_context_validationBuilderState = Data.Maybe.Just value }))

setEnableChangeReasonValidation :: Bool -> GetWorkspaceOutputBuilder ()
setEnableChangeReasonValidation value =
   Control.Monad.State.Strict.modify (\s -> (s { enable_change_reason_validationBuilderState = Data.Maybe.Just value }))

build :: GetWorkspaceOutputBuilder () -> Data.Either.Either Data.Text.Text GetWorkspaceOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.workspace_name is a required property.") Data.Either.Right (workspace_nameBuilderState st)
    organisation_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.organisation_id is a required property.") Data.Either.Right (organisation_idBuilderState st)
    organisation_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.organisation_name is a required property.") Data.Either.Right (organisation_nameBuilderState st)
    workspace_schema_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.workspace_schema_name is a required property.") Data.Either.Right (workspace_schema_nameBuilderState st)
    workspace_status' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.workspace_status is a required property.") Data.Either.Right (workspace_statusBuilderState st)
    workspace_admin_email' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.workspace_admin_email is a required property.") Data.Either.Right (workspace_admin_emailBuilderState st)
    config_version' <- Data.Either.Right (config_versionBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    mandatory_dimensions' <- Data.Either.Right (mandatory_dimensionsBuilderState st)
    metrics' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.metrics is a required property.") Data.Either.Right (metricsBuilderState st)
    allow_experiment_self_approval' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.allow_experiment_self_approval is a required property.") Data.Either.Right (allow_experiment_self_approvalBuilderState st)
    auto_populate_control' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.auto_populate_control is a required property.") Data.Either.Right (auto_populate_controlBuilderState st)
    enable_context_validation' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.enable_context_validation is a required property.") Data.Either.Right (enable_context_validationBuilderState st)
    enable_change_reason_validation' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceOutput.GetWorkspaceOutput.enable_change_reason_validation is a required property.") Data.Either.Right (enable_change_reason_validationBuilderState st)
    Data.Either.Right (GetWorkspaceOutput { 
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
        metrics = metrics',
        allow_experiment_self_approval = allow_experiment_self_approval',
        auto_populate_control = auto_populate_control',
        enable_context_validation = enable_context_validation',
        enable_change_reason_validation = enable_change_reason_validation'
    })


instance Io.Superposition.Utility.FromResponseParser GetWorkspaceOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "workspace_admin_email"
        var1 <- Io.Superposition.Utility.deSerField "auto_populate_control"
        var2 <- Io.Superposition.Utility.deSerField "enable_context_validation"
        var3 <- Io.Superposition.Utility.deSerField "created_at"
        var4 <- Io.Superposition.Utility.deSerField "organisation_name"
        var5 <- Io.Superposition.Utility.deSerField "last_modified_by"
        var6 <- Io.Superposition.Utility.deSerField "created_by"
        var7 <- Io.Superposition.Utility.deSerField "config_version"
        var8 <- Io.Superposition.Utility.deSerField "mandatory_dimensions"
        var9 <- Io.Superposition.Utility.deSerField "enable_change_reason_validation"
        var10 <- Io.Superposition.Utility.deSerField "workspace_status"
        var11 <- Io.Superposition.Utility.deSerField "last_modified_at"
        var12 <- Io.Superposition.Utility.deSerField "organisation_id"
        var13 <- Io.Superposition.Utility.deSerField "allow_experiment_self_approval"
        var14 <- Io.Superposition.Utility.deSerField "workspace_schema_name"
        var15 <- Io.Superposition.Utility.deSerField "metrics"
        var16 <- Io.Superposition.Utility.deSerField "workspace_name"
        pure $ GetWorkspaceOutput {
            workspace_name = var16,
            organisation_id = var12,
            organisation_name = var4,
            workspace_schema_name = var14,
            workspace_status = var10,
            workspace_admin_email = var0,
            config_version = var7,
            created_by = var6,
            last_modified_by = var5,
            last_modified_at = var11,
            created_at = var3,
            mandatory_dimensions = var8,
            metrics = var15,
            allow_experiment_self_approval = var13,
            auto_populate_control = var1,
            enable_context_validation = var2,
            enable_change_reason_validation = var9
        }


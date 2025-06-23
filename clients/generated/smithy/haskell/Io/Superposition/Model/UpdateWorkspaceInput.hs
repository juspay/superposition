module Io.Superposition.Model.UpdateWorkspaceInput (
    setOrgId,
    setWorkspaceName,
    setWorkspaceAdminEmail,
    setConfigVersion,
    setMandatoryDimensions,
    setWorkspaceStatus,
    setMetrics,
    setAllowExperimentSelfApproval,
    build,
    UpdateWorkspaceInputBuilder,
    UpdateWorkspaceInput,
    org_id,
    workspace_name,
    workspace_admin_email,
    config_version,
    mandatory_dimensions,
    workspace_status,
    metrics,
    allow_experiment_self_approval
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.WorkspaceStatus

data UpdateWorkspaceInput = UpdateWorkspaceInput {
    org_id :: Data.Text.Text,
    workspace_name :: Data.Text.Text,
    workspace_admin_email :: Data.Text.Text,
    config_version :: Data.Maybe.Maybe Data.Text.Text,
    mandatory_dimensions :: Data.Maybe.Maybe ([] Data.Text.Text),
    workspace_status :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus,
    metrics :: Data.Maybe.Maybe Data.Aeson.Value,
    allow_experiment_self_approval :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateWorkspaceInput where
    toJSON a = Data.Aeson.object [
        "org_id" Data.Aeson..= org_id a,
        "workspace_name" Data.Aeson..= workspace_name a,
        "workspace_admin_email" Data.Aeson..= workspace_admin_email a,
        "config_version" Data.Aeson..= config_version a,
        "mandatory_dimensions" Data.Aeson..= mandatory_dimensions a,
        "workspace_status" Data.Aeson..= workspace_status a,
        "metrics" Data.Aeson..= metrics a,
        "allow_experiment_self_approval" Data.Aeson..= allow_experiment_self_approval a
        ]
    


instance Data.Aeson.FromJSON UpdateWorkspaceInput where
    parseJSON = Data.Aeson.withObject "UpdateWorkspaceInput" $ \v -> UpdateWorkspaceInput
        Data.Functor.<$> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_name")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_admin_email")
        Control.Applicative.<*> (v Data.Aeson..: "config_version")
        Control.Applicative.<*> (v Data.Aeson..: "mandatory_dimensions")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_status")
        Control.Applicative.<*> (v Data.Aeson..: "metrics")
        Control.Applicative.<*> (v Data.Aeson..: "allow_experiment_self_approval")
    



data UpdateWorkspaceInputBuilderState = UpdateWorkspaceInputBuilderState {
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_admin_emailBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    config_versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    mandatory_dimensionsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    workspace_statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus,
    metricsBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    allow_experiment_self_approvalBuilderState :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateWorkspaceInputBuilderState
defaultBuilderState = UpdateWorkspaceInputBuilderState {
    org_idBuilderState = Data.Maybe.Nothing,
    workspace_nameBuilderState = Data.Maybe.Nothing,
    workspace_admin_emailBuilderState = Data.Maybe.Nothing,
    config_versionBuilderState = Data.Maybe.Nothing,
    mandatory_dimensionsBuilderState = Data.Maybe.Nothing,
    workspace_statusBuilderState = Data.Maybe.Nothing,
    metricsBuilderState = Data.Maybe.Nothing,
    allow_experiment_self_approvalBuilderState = Data.Maybe.Nothing
}

newtype UpdateWorkspaceInputBuilder a = UpdateWorkspaceInputBuilder {
    runUpdateWorkspaceInputBuilder :: UpdateWorkspaceInputBuilderState -> (UpdateWorkspaceInputBuilderState, a)
}

instance Data.Functor.Functor UpdateWorkspaceInputBuilder where
    fmap f (UpdateWorkspaceInputBuilder g) =
        UpdateWorkspaceInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateWorkspaceInputBuilder where
    pure a = UpdateWorkspaceInputBuilder (\s -> (s, a))
    (UpdateWorkspaceInputBuilder f) <*> (UpdateWorkspaceInputBuilder g) = UpdateWorkspaceInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateWorkspaceInputBuilder where
    (UpdateWorkspaceInputBuilder f) >>= g = UpdateWorkspaceInputBuilder (\s ->
        let (s', a) = f s
            (UpdateWorkspaceInputBuilder h) = g a
        in h s')

setOrgId :: Data.Text.Text -> UpdateWorkspaceInputBuilder ()
setOrgId value =
   UpdateWorkspaceInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setWorkspaceName :: Data.Text.Text -> UpdateWorkspaceInputBuilder ()
setWorkspaceName value =
   UpdateWorkspaceInputBuilder (\s -> (s { workspace_nameBuilderState = Data.Maybe.Just value }, ()))

setWorkspaceAdminEmail :: Data.Text.Text -> UpdateWorkspaceInputBuilder ()
setWorkspaceAdminEmail value =
   UpdateWorkspaceInputBuilder (\s -> (s { workspace_admin_emailBuilderState = Data.Maybe.Just value }, ()))

setConfigVersion :: Data.Maybe.Maybe Data.Text.Text -> UpdateWorkspaceInputBuilder ()
setConfigVersion value =
   UpdateWorkspaceInputBuilder (\s -> (s { config_versionBuilderState = value }, ()))

setMandatoryDimensions :: Data.Maybe.Maybe ([] Data.Text.Text) -> UpdateWorkspaceInputBuilder ()
setMandatoryDimensions value =
   UpdateWorkspaceInputBuilder (\s -> (s { mandatory_dimensionsBuilderState = value }, ()))

setWorkspaceStatus :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus -> UpdateWorkspaceInputBuilder ()
setWorkspaceStatus value =
   UpdateWorkspaceInputBuilder (\s -> (s { workspace_statusBuilderState = value }, ()))

setMetrics :: Data.Maybe.Maybe Data.Aeson.Value -> UpdateWorkspaceInputBuilder ()
setMetrics value =
   UpdateWorkspaceInputBuilder (\s -> (s { metricsBuilderState = value }, ()))

setAllowExperimentSelfApproval :: Data.Maybe.Maybe Bool -> UpdateWorkspaceInputBuilder ()
setAllowExperimentSelfApproval value =
   UpdateWorkspaceInputBuilder (\s -> (s { allow_experiment_self_approvalBuilderState = value }, ()))

build :: UpdateWorkspaceInputBuilder () -> Data.Either.Either Data.Text.Text UpdateWorkspaceInput
build builder = do
    let (st, _) = runUpdateWorkspaceInputBuilder builder defaultBuilderState
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceInput.UpdateWorkspaceInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    workspace_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceInput.UpdateWorkspaceInput.workspace_name is a required property.") Data.Either.Right (workspace_nameBuilderState st)
    workspace_admin_email' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceInput.UpdateWorkspaceInput.workspace_admin_email is a required property.") Data.Either.Right (workspace_admin_emailBuilderState st)
    config_version' <- Data.Either.Right (config_versionBuilderState st)
    mandatory_dimensions' <- Data.Either.Right (mandatory_dimensionsBuilderState st)
    workspace_status' <- Data.Either.Right (workspace_statusBuilderState st)
    metrics' <- Data.Either.Right (metricsBuilderState st)
    allow_experiment_self_approval' <- Data.Either.Right (allow_experiment_self_approvalBuilderState st)
    Data.Either.Right (UpdateWorkspaceInput { 
        org_id = org_id',
        workspace_name = workspace_name',
        workspace_admin_email = workspace_admin_email',
        config_version = config_version',
        mandatory_dimensions = mandatory_dimensions',
        workspace_status = workspace_status',
        metrics = metrics',
        allow_experiment_self_approval = allow_experiment_self_approval'
    })



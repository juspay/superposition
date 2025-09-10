module Io.Superposition.Model.CreateWorkspaceInput (
    setOrgId,
    setWorkspaceAdminEmail,
    setWorkspaceName,
    setWorkspaceStatus,
    setStrictMode,
    setMetrics,
    setAllowExperimentSelfApproval,
    setAutoPopulateControl,
    build,
    CreateWorkspaceInputBuilder,
    CreateWorkspaceInput,
    org_id,
    workspace_admin_email,
    workspace_name,
    workspace_status,
    strict_mode,
    metrics,
    allow_experiment_self_approval,
    auto_populate_control
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
import qualified Io.Superposition.Model.WorkspaceStatus
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data CreateWorkspaceInput = CreateWorkspaceInput {
    org_id :: Data.Text.Text,
    workspace_admin_email :: Data.Text.Text,
    workspace_name :: Data.Text.Text,
    workspace_status :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus,
    strict_mode :: Bool,
    metrics :: Data.Maybe.Maybe Data.Aeson.Value,
    allow_experiment_self_approval :: Bool,
    auto_populate_control :: Bool
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateWorkspaceInput where
    toJSON a = Data.Aeson.object [
        "org_id" Data.Aeson..= org_id a,
        "workspace_admin_email" Data.Aeson..= workspace_admin_email a,
        "workspace_name" Data.Aeson..= workspace_name a,
        "workspace_status" Data.Aeson..= workspace_status a,
        "strict_mode" Data.Aeson..= strict_mode a,
        "metrics" Data.Aeson..= metrics a,
        "allow_experiment_self_approval" Data.Aeson..= allow_experiment_self_approval a,
        "auto_populate_control" Data.Aeson..= auto_populate_control a
        ]
    

instance Io.Superposition.Utility.SerializeBody CreateWorkspaceInput

instance Data.Aeson.FromJSON CreateWorkspaceInput where
    parseJSON = Data.Aeson.withObject "CreateWorkspaceInput" $ \v -> CreateWorkspaceInput
        Data.Functor.<$> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_admin_email")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_name")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_status")
        Control.Applicative.<*> (v Data.Aeson..: "strict_mode")
        Control.Applicative.<*> (v Data.Aeson..: "metrics")
        Control.Applicative.<*> (v Data.Aeson..: "allow_experiment_self_approval")
        Control.Applicative.<*> (v Data.Aeson..: "auto_populate_control")
    



data CreateWorkspaceInputBuilderState = CreateWorkspaceInputBuilderState {
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_admin_emailBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus,
    strict_modeBuilderState :: Data.Maybe.Maybe Bool,
    metricsBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    allow_experiment_self_approvalBuilderState :: Data.Maybe.Maybe Bool,
    auto_populate_controlBuilderState :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateWorkspaceInputBuilderState
defaultBuilderState = CreateWorkspaceInputBuilderState {
    org_idBuilderState = Data.Maybe.Nothing,
    workspace_admin_emailBuilderState = Data.Maybe.Nothing,
    workspace_nameBuilderState = Data.Maybe.Nothing,
    workspace_statusBuilderState = Data.Maybe.Nothing,
    strict_modeBuilderState = Data.Maybe.Nothing,
    metricsBuilderState = Data.Maybe.Nothing,
    allow_experiment_self_approvalBuilderState = Data.Maybe.Nothing,
    auto_populate_controlBuilderState = Data.Maybe.Nothing
}

type CreateWorkspaceInputBuilder = Control.Monad.State.Strict.State CreateWorkspaceInputBuilderState

setOrgId :: Data.Text.Text -> CreateWorkspaceInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setWorkspaceAdminEmail :: Data.Text.Text -> CreateWorkspaceInputBuilder ()
setWorkspaceAdminEmail value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_admin_emailBuilderState = Data.Maybe.Just value }))

setWorkspaceName :: Data.Text.Text -> CreateWorkspaceInputBuilder ()
setWorkspaceName value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_nameBuilderState = Data.Maybe.Just value }))

setWorkspaceStatus :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus -> CreateWorkspaceInputBuilder ()
setWorkspaceStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_statusBuilderState = value }))

setStrictMode :: Bool -> CreateWorkspaceInputBuilder ()
setStrictMode value =
   Control.Monad.State.Strict.modify (\s -> (s { strict_modeBuilderState = Data.Maybe.Just value }))

setMetrics :: Data.Maybe.Maybe Data.Aeson.Value -> CreateWorkspaceInputBuilder ()
setMetrics value =
   Control.Monad.State.Strict.modify (\s -> (s { metricsBuilderState = value }))

setAllowExperimentSelfApproval :: Bool -> CreateWorkspaceInputBuilder ()
setAllowExperimentSelfApproval value =
   Control.Monad.State.Strict.modify (\s -> (s { allow_experiment_self_approvalBuilderState = Data.Maybe.Just value }))

setAutoPopulateControl :: Bool -> CreateWorkspaceInputBuilder ()
setAutoPopulateControl value =
   Control.Monad.State.Strict.modify (\s -> (s { auto_populate_controlBuilderState = Data.Maybe.Just value }))

build :: CreateWorkspaceInputBuilder () -> Data.Either.Either Data.Text.Text CreateWorkspaceInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    workspace_admin_email' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.workspace_admin_email is a required property.") Data.Either.Right (workspace_admin_emailBuilderState st)
    workspace_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.workspace_name is a required property.") Data.Either.Right (workspace_nameBuilderState st)
    workspace_status' <- Data.Either.Right (workspace_statusBuilderState st)
    strict_mode' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.strict_mode is a required property.") Data.Either.Right (strict_modeBuilderState st)
    metrics' <- Data.Either.Right (metricsBuilderState st)
    allow_experiment_self_approval' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.allow_experiment_self_approval is a required property.") Data.Either.Right (allow_experiment_self_approvalBuilderState st)
    auto_populate_control' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.auto_populate_control is a required property.") Data.Either.Right (auto_populate_controlBuilderState st)
    Data.Either.Right (CreateWorkspaceInput { 
        org_id = org_id',
        workspace_admin_email = workspace_admin_email',
        workspace_name = workspace_name',
        workspace_status = workspace_status',
        strict_mode = strict_mode',
        metrics = metrics',
        allow_experiment_self_approval = allow_experiment_self_approval',
        auto_populate_control = auto_populate_control'
    })


instance Io.Superposition.Utility.IntoRequestBuilder CreateWorkspaceInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "workspaces"
            ]
        
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "allow_experiment_self_approval" (allow_experiment_self_approval self)
        Io.Superposition.Utility.serField "workspace_admin_email" (workspace_admin_email self)
        Io.Superposition.Utility.serField "strict_mode" (strict_mode self)
        Io.Superposition.Utility.serField "auto_populate_control" (auto_populate_control self)
        Io.Superposition.Utility.serField "metrics" (metrics self)
        Io.Superposition.Utility.serField "workspace_name" (workspace_name self)
        Io.Superposition.Utility.serField "workspace_status" (workspace_status self)


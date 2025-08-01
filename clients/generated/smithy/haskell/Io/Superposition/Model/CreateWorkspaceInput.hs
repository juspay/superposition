module Io.Superposition.Model.CreateWorkspaceInput (
    setOrgId,
    setWorkspaceAdminEmail,
    setWorkspaceName,
    setWorkspaceStatus,
    setStrictMode,
    setMetrics,
    setAllowExperimentSelfApproval,
    build,
    CreateWorkspaceInputBuilder,
    CreateWorkspaceInput,
    org_id,
    workspace_admin_email,
    workspace_name,
    workspace_status,
    strict_mode,
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

data CreateWorkspaceInput = CreateWorkspaceInput {
    org_id :: Data.Text.Text,
    workspace_admin_email :: Data.Text.Text,
    workspace_name :: Data.Text.Text,
    workspace_status :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus,
    strict_mode :: Bool,
    metrics :: Data.Maybe.Maybe Data.Aeson.Value,
    allow_experiment_self_approval :: Bool
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
        "allow_experiment_self_approval" Data.Aeson..= allow_experiment_self_approval a
        ]
    


instance Data.Aeson.FromJSON CreateWorkspaceInput where
    parseJSON = Data.Aeson.withObject "CreateWorkspaceInput" $ \v -> CreateWorkspaceInput
        Data.Functor.<$> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_admin_email")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_name")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_status")
        Control.Applicative.<*> (v Data.Aeson..: "strict_mode")
        Control.Applicative.<*> (v Data.Aeson..: "metrics")
        Control.Applicative.<*> (v Data.Aeson..: "allow_experiment_self_approval")
    



data CreateWorkspaceInputBuilderState = CreateWorkspaceInputBuilderState {
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_admin_emailBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus,
    strict_modeBuilderState :: Data.Maybe.Maybe Bool,
    metricsBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    allow_experiment_self_approvalBuilderState :: Data.Maybe.Maybe Bool
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
    allow_experiment_self_approvalBuilderState = Data.Maybe.Nothing
}

newtype CreateWorkspaceInputBuilder a = CreateWorkspaceInputBuilder {
    runCreateWorkspaceInputBuilder :: CreateWorkspaceInputBuilderState -> (CreateWorkspaceInputBuilderState, a)
}

instance Data.Functor.Functor CreateWorkspaceInputBuilder where
    fmap f (CreateWorkspaceInputBuilder g) =
        CreateWorkspaceInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CreateWorkspaceInputBuilder where
    pure a = CreateWorkspaceInputBuilder (\s -> (s, a))
    (CreateWorkspaceInputBuilder f) <*> (CreateWorkspaceInputBuilder g) = CreateWorkspaceInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CreateWorkspaceInputBuilder where
    (CreateWorkspaceInputBuilder f) >>= g = CreateWorkspaceInputBuilder (\s ->
        let (s', a) = f s
            (CreateWorkspaceInputBuilder h) = g a
        in h s')

setOrgId :: Data.Text.Text -> CreateWorkspaceInputBuilder ()
setOrgId value =
   CreateWorkspaceInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setWorkspaceAdminEmail :: Data.Text.Text -> CreateWorkspaceInputBuilder ()
setWorkspaceAdminEmail value =
   CreateWorkspaceInputBuilder (\s -> (s { workspace_admin_emailBuilderState = Data.Maybe.Just value }, ()))

setWorkspaceName :: Data.Text.Text -> CreateWorkspaceInputBuilder ()
setWorkspaceName value =
   CreateWorkspaceInputBuilder (\s -> (s { workspace_nameBuilderState = Data.Maybe.Just value }, ()))

setWorkspaceStatus :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus -> CreateWorkspaceInputBuilder ()
setWorkspaceStatus value =
   CreateWorkspaceInputBuilder (\s -> (s { workspace_statusBuilderState = value }, ()))

setStrictMode :: Bool -> CreateWorkspaceInputBuilder ()
setStrictMode value =
   CreateWorkspaceInputBuilder (\s -> (s { strict_modeBuilderState = Data.Maybe.Just value }, ()))

setMetrics :: Data.Maybe.Maybe Data.Aeson.Value -> CreateWorkspaceInputBuilder ()
setMetrics value =
   CreateWorkspaceInputBuilder (\s -> (s { metricsBuilderState = value }, ()))

setAllowExperimentSelfApproval :: Bool -> CreateWorkspaceInputBuilder ()
setAllowExperimentSelfApproval value =
   CreateWorkspaceInputBuilder (\s -> (s { allow_experiment_self_approvalBuilderState = Data.Maybe.Just value }, ()))

build :: CreateWorkspaceInputBuilder () -> Data.Either.Either Data.Text.Text CreateWorkspaceInput
build builder = do
    let (st, _) = runCreateWorkspaceInputBuilder builder defaultBuilderState
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    workspace_admin_email' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.workspace_admin_email is a required property.") Data.Either.Right (workspace_admin_emailBuilderState st)
    workspace_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.workspace_name is a required property.") Data.Either.Right (workspace_nameBuilderState st)
    workspace_status' <- Data.Either.Right (workspace_statusBuilderState st)
    strict_mode' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.strict_mode is a required property.") Data.Either.Right (strict_modeBuilderState st)
    metrics' <- Data.Either.Right (metricsBuilderState st)
    allow_experiment_self_approval' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWorkspaceInput.CreateWorkspaceInput.allow_experiment_self_approval is a required property.") Data.Either.Right (allow_experiment_self_approvalBuilderState st)
    Data.Either.Right (CreateWorkspaceInput { 
        org_id = org_id',
        workspace_admin_email = workspace_admin_email',
        workspace_name = workspace_name',
        workspace_status = workspace_status',
        strict_mode = strict_mode',
        metrics = metrics',
        allow_experiment_self_approval = allow_experiment_self_approval'
    })



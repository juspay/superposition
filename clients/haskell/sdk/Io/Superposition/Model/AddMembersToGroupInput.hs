module Io.Superposition.Model.AddMembersToGroupInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setChangeReason,
    setMemberExperimentIds,
    build,
    AddMembersToGroupInputBuilder,
    AddMembersToGroupInput,
    workspace_id,
    org_id,
    id',
    change_reason,
    member_experiment_ids
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
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data AddMembersToGroupInput = AddMembersToGroupInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    member_experiment_ids :: [] Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON AddMembersToGroupInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "change_reason" Data.Aeson..= change_reason a,
        "member_experiment_ids" Data.Aeson..= member_experiment_ids a
        ]
    

instance Io.Superposition.Utility.SerializeBody AddMembersToGroupInput

instance Data.Aeson.FromJSON AddMembersToGroupInput where
    parseJSON = Data.Aeson.withObject "AddMembersToGroupInput" $ \v -> AddMembersToGroupInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "member_experiment_ids")
    



data AddMembersToGroupInputBuilderState = AddMembersToGroupInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    member_experiment_idsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: AddMembersToGroupInputBuilderState
defaultBuilderState = AddMembersToGroupInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    member_experiment_idsBuilderState = Data.Maybe.Nothing
}

type AddMembersToGroupInputBuilder = Control.Monad.State.Strict.State AddMembersToGroupInputBuilderState

setWorkspaceId :: Data.Text.Text -> AddMembersToGroupInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> AddMembersToGroupInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setId' :: Data.Text.Text -> AddMembersToGroupInputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> AddMembersToGroupInputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setMemberExperimentIds :: [] Data.Text.Text -> AddMembersToGroupInputBuilder ()
setMemberExperimentIds value =
   Control.Monad.State.Strict.modify (\s -> (s { member_experiment_idsBuilderState = Data.Maybe.Just value }))

build :: AddMembersToGroupInputBuilder () -> Data.Either.Either Data.Text.Text AddMembersToGroupInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupInput.AddMembersToGroupInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupInput.AddMembersToGroupInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupInput.AddMembersToGroupInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupInput.AddMembersToGroupInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    member_experiment_ids' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupInput.AddMembersToGroupInput.member_experiment_ids is a required property.") Data.Either.Right (member_experiment_idsBuilderState st)
    Data.Either.Right (AddMembersToGroupInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        change_reason = change_reason',
        member_experiment_ids = member_experiment_ids'
    })


instance Io.Superposition.Utility.IntoRequestBuilder AddMembersToGroupInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPatch
        Io.Superposition.Utility.setPath [
            "experiment-groups",
            Io.Superposition.Utility.serializeElement (id' self),
            "add-members"
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "change_reason" (change_reason self)
        Io.Superposition.Utility.serField "member_experiment_ids" (member_experiment_ids self)


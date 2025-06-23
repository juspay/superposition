module Io.Superposition.Model.RemoveMembersFromGroupInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setChangeReason,
    setMemberExperimentIds,
    build,
    RemoveMembersFromGroupInputBuilder,
    RemoveMembersFromGroupInput,
    workspace_id,
    org_id,
    id',
    change_reason,
    member_experiment_ids
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

data RemoveMembersFromGroupInput = RemoveMembersFromGroupInput {
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

instance Data.Aeson.ToJSON RemoveMembersFromGroupInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "change_reason" Data.Aeson..= change_reason a,
        "member_experiment_ids" Data.Aeson..= member_experiment_ids a
        ]
    


instance Data.Aeson.FromJSON RemoveMembersFromGroupInput where
    parseJSON = Data.Aeson.withObject "RemoveMembersFromGroupInput" $ \v -> RemoveMembersFromGroupInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "member_experiment_ids")
    



data RemoveMembersFromGroupInputBuilderState = RemoveMembersFromGroupInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    member_experiment_idsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: RemoveMembersFromGroupInputBuilderState
defaultBuilderState = RemoveMembersFromGroupInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    member_experiment_idsBuilderState = Data.Maybe.Nothing
}

newtype RemoveMembersFromGroupInputBuilder a = RemoveMembersFromGroupInputBuilder {
    runRemoveMembersFromGroupInputBuilder :: RemoveMembersFromGroupInputBuilderState -> (RemoveMembersFromGroupInputBuilderState, a)
}

instance Data.Functor.Functor RemoveMembersFromGroupInputBuilder where
    fmap f (RemoveMembersFromGroupInputBuilder g) =
        RemoveMembersFromGroupInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative RemoveMembersFromGroupInputBuilder where
    pure a = RemoveMembersFromGroupInputBuilder (\s -> (s, a))
    (RemoveMembersFromGroupInputBuilder f) <*> (RemoveMembersFromGroupInputBuilder g) = RemoveMembersFromGroupInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad RemoveMembersFromGroupInputBuilder where
    (RemoveMembersFromGroupInputBuilder f) >>= g = RemoveMembersFromGroupInputBuilder (\s ->
        let (s', a) = f s
            (RemoveMembersFromGroupInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> RemoveMembersFromGroupInputBuilder ()
setWorkspaceId value =
   RemoveMembersFromGroupInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> RemoveMembersFromGroupInputBuilder ()
setOrgId value =
   RemoveMembersFromGroupInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setId' :: Data.Text.Text -> RemoveMembersFromGroupInputBuilder ()
setId' value =
   RemoveMembersFromGroupInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> RemoveMembersFromGroupInputBuilder ()
setChangeReason value =
   RemoveMembersFromGroupInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setMemberExperimentIds :: [] Data.Text.Text -> RemoveMembersFromGroupInputBuilder ()
setMemberExperimentIds value =
   RemoveMembersFromGroupInputBuilder (\s -> (s { member_experiment_idsBuilderState = Data.Maybe.Just value }, ()))

build :: RemoveMembersFromGroupInputBuilder () -> Data.Either.Either Data.Text.Text RemoveMembersFromGroupInput
build builder = do
    let (st, _) = runRemoveMembersFromGroupInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupInput.RemoveMembersFromGroupInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupInput.RemoveMembersFromGroupInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupInput.RemoveMembersFromGroupInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupInput.RemoveMembersFromGroupInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    member_experiment_ids' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupInput.RemoveMembersFromGroupInput.member_experiment_ids is a required property.") Data.Either.Right (member_experiment_idsBuilderState st)
    Data.Either.Right (RemoveMembersFromGroupInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        change_reason = change_reason',
        member_experiment_ids = member_experiment_ids'
    })



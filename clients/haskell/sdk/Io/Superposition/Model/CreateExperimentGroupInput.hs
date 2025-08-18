module Io.Superposition.Model.CreateExperimentGroupInput (
    setWorkspaceId,
    setOrgId,
    setName,
    setDescription,
    setChangeReason,
    setContext,
    setTrafficPercentage,
    setMemberExperimentIds,
    build,
    CreateExperimentGroupInputBuilder,
    CreateExperimentGroupInput,
    workspace_id,
    org_id,
    name,
    description,
    change_reason,
    context,
    traffic_percentage,
    member_experiment_ids
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data CreateExperimentGroupInput = CreateExperimentGroupInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    name :: Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    traffic_percentage :: Data.Int.Int32,
    member_experiment_ids :: Data.Maybe.Maybe ([] Data.Text.Text)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateExperimentGroupInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "name" Data.Aeson..= name a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "context" Data.Aeson..= context a,
        "traffic_percentage" Data.Aeson..= traffic_percentage a,
        "member_experiment_ids" Data.Aeson..= member_experiment_ids a
        ]
    

instance Io.Superposition.Utility.SerializeBody CreateExperimentGroupInput

instance Data.Aeson.FromJSON CreateExperimentGroupInput where
    parseJSON = Data.Aeson.withObject "CreateExperimentGroupInput" $ \v -> CreateExperimentGroupInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "traffic_percentage")
        Control.Applicative.<*> (v Data.Aeson..: "member_experiment_ids")
    



data CreateExperimentGroupInputBuilderState = CreateExperimentGroupInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    traffic_percentageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    member_experiment_idsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateExperimentGroupInputBuilderState
defaultBuilderState = CreateExperimentGroupInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing,
    traffic_percentageBuilderState = Data.Maybe.Nothing,
    member_experiment_idsBuilderState = Data.Maybe.Nothing
}

type CreateExperimentGroupInputBuilder = Control.Monad.State.Strict.State CreateExperimentGroupInputBuilderState

setWorkspaceId :: Data.Text.Text -> CreateExperimentGroupInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> CreateExperimentGroupInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> CreateExperimentGroupInputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> CreateExperimentGroupInputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> CreateExperimentGroupInputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> CreateExperimentGroupInputBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = Data.Maybe.Just value }))

setTrafficPercentage :: Data.Int.Int32 -> CreateExperimentGroupInputBuilder ()
setTrafficPercentage value =
   Control.Monad.State.Strict.modify (\s -> (s { traffic_percentageBuilderState = Data.Maybe.Just value }))

setMemberExperimentIds :: Data.Maybe.Maybe ([] Data.Text.Text) -> CreateExperimentGroupInputBuilder ()
setMemberExperimentIds value =
   Control.Monad.State.Strict.modify (\s -> (s { member_experiment_idsBuilderState = value }))

build :: CreateExperimentGroupInputBuilder () -> Data.Either.Either Data.Text.Text CreateExperimentGroupInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInput.context is a required property.") Data.Either.Right (contextBuilderState st)
    traffic_percentage' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInput.traffic_percentage is a required property.") Data.Either.Right (traffic_percentageBuilderState st)
    member_experiment_ids' <- Data.Either.Right (member_experiment_idsBuilderState st)
    Data.Either.Right (CreateExperimentGroupInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        name = name',
        description = description',
        change_reason = change_reason',
        context = context',
        traffic_percentage = traffic_percentage',
        member_experiment_ids = member_experiment_ids'
    })


instance Io.Superposition.Utility.IntoRequestBuilder CreateExperimentGroupInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "experiment-groups"
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "change_reason" (change_reason self)
        Io.Superposition.Utility.serField "traffic_percentage" (traffic_percentage self)
        Io.Superposition.Utility.serField "name" (name self)
        Io.Superposition.Utility.serField "context" (context self)
        Io.Superposition.Utility.serField "description" (description self)
        Io.Superposition.Utility.serField "member_experiment_ids" (member_experiment_ids self)


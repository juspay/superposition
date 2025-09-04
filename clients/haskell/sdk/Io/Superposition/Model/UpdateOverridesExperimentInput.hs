module Io.Superposition.Model.UpdateOverridesExperimentInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setVariantList,
    setDescription,
    setChangeReason,
    setMetrics,
    setExperimentGroupId,
    build,
    UpdateOverridesExperimentInputBuilder,
    UpdateOverridesExperimentInput,
    workspace_id,
    org_id,
    id',
    variant_list,
    description,
    change_reason,
    metrics,
    experiment_group_id
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
import qualified Io.Superposition.Model.VariantUpdateRequest
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data UpdateOverridesExperimentInput = UpdateOverridesExperimentInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    variant_list :: [] Io.Superposition.Model.VariantUpdateRequest.VariantUpdateRequest,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Text.Text,
    metrics :: Data.Maybe.Maybe Data.Aeson.Value,
    experiment_group_id :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateOverridesExperimentInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "variant_list" Data.Aeson..= variant_list a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "metrics" Data.Aeson..= metrics a,
        "experiment_group_id" Data.Aeson..= experiment_group_id a
        ]
    

instance Io.Superposition.Utility.SerializeBody UpdateOverridesExperimentInput

instance Data.Aeson.FromJSON UpdateOverridesExperimentInput where
    parseJSON = Data.Aeson.withObject "UpdateOverridesExperimentInput" $ \v -> UpdateOverridesExperimentInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "variant_list")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "metrics")
        Control.Applicative.<*> (v Data.Aeson..: "experiment_group_id")
    



data UpdateOverridesExperimentInputBuilderState = UpdateOverridesExperimentInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    variant_listBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.VariantUpdateRequest.VariantUpdateRequest),
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    metricsBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    experiment_group_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateOverridesExperimentInputBuilderState
defaultBuilderState = UpdateOverridesExperimentInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    variant_listBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    metricsBuilderState = Data.Maybe.Nothing,
    experiment_group_idBuilderState = Data.Maybe.Nothing
}

type UpdateOverridesExperimentInputBuilder = Control.Monad.State.Strict.State UpdateOverridesExperimentInputBuilderState

setWorkspaceId :: Data.Text.Text -> UpdateOverridesExperimentInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> UpdateOverridesExperimentInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setId' :: Data.Text.Text -> UpdateOverridesExperimentInputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setVariantList :: [] Io.Superposition.Model.VariantUpdateRequest.VariantUpdateRequest -> UpdateOverridesExperimentInputBuilder ()
setVariantList value =
   Control.Monad.State.Strict.modify (\s -> (s { variant_listBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> UpdateOverridesExperimentInputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = value }))

setChangeReason :: Data.Text.Text -> UpdateOverridesExperimentInputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setMetrics :: Data.Maybe.Maybe Data.Aeson.Value -> UpdateOverridesExperimentInputBuilder ()
setMetrics value =
   Control.Monad.State.Strict.modify (\s -> (s { metricsBuilderState = value }))

setExperimentGroupId :: Data.Maybe.Maybe Data.Text.Text -> UpdateOverridesExperimentInputBuilder ()
setExperimentGroupId value =
   Control.Monad.State.Strict.modify (\s -> (s { experiment_group_idBuilderState = value }))

build :: UpdateOverridesExperimentInputBuilder () -> Data.Either.Either Data.Text.Text UpdateOverridesExperimentInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverridesExperimentInput.UpdateOverridesExperimentInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverridesExperimentInput.UpdateOverridesExperimentInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverridesExperimentInput.UpdateOverridesExperimentInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    variant_list' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverridesExperimentInput.UpdateOverridesExperimentInput.variant_list is a required property.") Data.Either.Right (variant_listBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverridesExperimentInput.UpdateOverridesExperimentInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    metrics' <- Data.Either.Right (metricsBuilderState st)
    experiment_group_id' <- Data.Either.Right (experiment_group_idBuilderState st)
    Data.Either.Right (UpdateOverridesExperimentInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        variant_list = variant_list',
        description = description',
        change_reason = change_reason',
        metrics = metrics',
        experiment_group_id = experiment_group_id'
    })


instance Io.Superposition.Utility.IntoRequestBuilder UpdateOverridesExperimentInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPut
        Io.Superposition.Utility.setPath [
            "experiments",
            Io.Superposition.Utility.serializeElement (id' self),
            "overrides"
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "change_reason" (change_reason self)
        Io.Superposition.Utility.serField "variant_list" (variant_list self)
        Io.Superposition.Utility.serField "description" (description self)
        Io.Superposition.Utility.serField "experiment_group_id" (experiment_group_id self)
        Io.Superposition.Utility.serField "metrics" (metrics self)


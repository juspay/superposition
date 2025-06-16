module Io.Superposition.Model.UpdateExperimentGroupInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setChangeReason,
    setDescription,
    setTrafficPercentage,
    build,
    UpdateExperimentGroupInputBuilder,
    UpdateExperimentGroupInput,
    workspace_id,
    org_id,
    id',
    change_reason,
    description,
    traffic_percentage
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

data UpdateExperimentGroupInput = UpdateExperimentGroupInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    traffic_percentage :: Data.Maybe.Maybe Integer
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateExperimentGroupInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "change_reason" Data.Aeson..= change_reason a,
        "description" Data.Aeson..= description a,
        "traffic_percentage" Data.Aeson..= traffic_percentage a
        ]
    


instance Data.Aeson.FromJSON UpdateExperimentGroupInput where
    parseJSON = Data.Aeson.withObject "UpdateExperimentGroupInput" $ \v -> UpdateExperimentGroupInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "traffic_percentage")
    



data UpdateExperimentGroupInputBuilderState = UpdateExperimentGroupInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    traffic_percentageBuilderState :: Data.Maybe.Maybe Integer
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateExperimentGroupInputBuilderState
defaultBuilderState = UpdateExperimentGroupInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    traffic_percentageBuilderState = Data.Maybe.Nothing
}

newtype UpdateExperimentGroupInputBuilder a = UpdateExperimentGroupInputBuilder {
    runUpdateExperimentGroupInputBuilder :: UpdateExperimentGroupInputBuilderState -> (UpdateExperimentGroupInputBuilderState, a)
}

instance Data.Functor.Functor UpdateExperimentGroupInputBuilder where
    fmap f (UpdateExperimentGroupInputBuilder g) =
        UpdateExperimentGroupInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateExperimentGroupInputBuilder where
    pure a = UpdateExperimentGroupInputBuilder (\s -> (s, a))
    (UpdateExperimentGroupInputBuilder f) <*> (UpdateExperimentGroupInputBuilder g) = UpdateExperimentGroupInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateExperimentGroupInputBuilder where
    (UpdateExperimentGroupInputBuilder f) >>= g = UpdateExperimentGroupInputBuilder (\s ->
        let (s', a) = f s
            (UpdateExperimentGroupInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> UpdateExperimentGroupInputBuilder ()
setWorkspaceId value =
   UpdateExperimentGroupInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> UpdateExperimentGroupInputBuilder ()
setOrgId value =
   UpdateExperimentGroupInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setId' :: Data.Text.Text -> UpdateExperimentGroupInputBuilder ()
setId' value =
   UpdateExperimentGroupInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> UpdateExperimentGroupInputBuilder ()
setChangeReason value =
   UpdateExperimentGroupInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> UpdateExperimentGroupInputBuilder ()
setDescription value =
   UpdateExperimentGroupInputBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setTrafficPercentage :: Data.Maybe.Maybe Integer -> UpdateExperimentGroupInputBuilder ()
setTrafficPercentage value =
   UpdateExperimentGroupInputBuilder (\s -> (s { traffic_percentageBuilderState = value }, ()))

build :: UpdateExperimentGroupInputBuilder () -> Data.Either.Either Data.Text.Text UpdateExperimentGroupInput
build builder = do
    let (st, _) = runUpdateExperimentGroupInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateExperimentGroupInput.UpdateExperimentGroupInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateExperimentGroupInput.UpdateExperimentGroupInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateExperimentGroupInput.UpdateExperimentGroupInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateExperimentGroupInput.UpdateExperimentGroupInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    traffic_percentage' <- Data.Either.Right (traffic_percentageBuilderState st)
    Data.Either.Right (UpdateExperimentGroupInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        change_reason = change_reason',
        description = description',
        traffic_percentage = traffic_percentage'
    })



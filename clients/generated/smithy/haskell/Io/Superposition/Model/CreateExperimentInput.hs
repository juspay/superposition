module Io.Superposition.Model.CreateExperimentInput (
    setWorkspaceId,
    setOrgId,
    setName,
    setExperimentType,
    setContext,
    setVariants,
    setDescription,
    setChangeReason,
    setMetrics,
    build,
    CreateExperimentInputBuilder,
    CreateExperimentInput,
    workspace_id,
    org_id,
    name,
    experiment_type,
    context,
    variants,
    description,
    change_reason,
    metrics
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ExperimentType
import qualified Io.Superposition.Model.Variant

data CreateExperimentInput = CreateExperimentInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    name :: Data.Text.Text,
    experiment_type :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentType.ExperimentType,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    variants :: [] Io.Superposition.Model.Variant.Variant,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    metrics :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateExperimentInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "name" Data.Aeson..= name a,
        "experiment_type" Data.Aeson..= experiment_type a,
        "context" Data.Aeson..= context a,
        "variants" Data.Aeson..= variants a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "metrics" Data.Aeson..= metrics a
        ]
    


instance Data.Aeson.FromJSON CreateExperimentInput where
    parseJSON = Data.Aeson.withObject "CreateExperimentInput" $ \v -> CreateExperimentInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "experiment_type")
        Control.Applicative.<*> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "variants")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "metrics")
    



data CreateExperimentInputBuilderState = CreateExperimentInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    experiment_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentType.ExperimentType,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    variantsBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.Variant.Variant),
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    metricsBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateExperimentInputBuilderState
defaultBuilderState = CreateExperimentInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    experiment_typeBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing,
    variantsBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    metricsBuilderState = Data.Maybe.Nothing
}

newtype CreateExperimentInputBuilder a = CreateExperimentInputBuilder {
    runCreateExperimentInputBuilder :: CreateExperimentInputBuilderState -> (CreateExperimentInputBuilderState, a)
}

instance Data.Functor.Functor CreateExperimentInputBuilder where
    fmap f (CreateExperimentInputBuilder g) =
        CreateExperimentInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CreateExperimentInputBuilder where
    pure a = CreateExperimentInputBuilder (\s -> (s, a))
    (CreateExperimentInputBuilder f) <*> (CreateExperimentInputBuilder g) = CreateExperimentInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CreateExperimentInputBuilder where
    (CreateExperimentInputBuilder f) >>= g = CreateExperimentInputBuilder (\s ->
        let (s', a) = f s
            (CreateExperimentInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> CreateExperimentInputBuilder ()
setWorkspaceId value =
   CreateExperimentInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> CreateExperimentInputBuilder ()
setOrgId value =
   CreateExperimentInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setName :: Data.Text.Text -> CreateExperimentInputBuilder ()
setName value =
   CreateExperimentInputBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

setExperimentType :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentType.ExperimentType -> CreateExperimentInputBuilder ()
setExperimentType value =
   CreateExperimentInputBuilder (\s -> (s { experiment_typeBuilderState = value }, ()))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> CreateExperimentInputBuilder ()
setContext value =
   CreateExperimentInputBuilder (\s -> (s { contextBuilderState = Data.Maybe.Just value }, ()))

setVariants :: [] Io.Superposition.Model.Variant.Variant -> CreateExperimentInputBuilder ()
setVariants value =
   CreateExperimentInputBuilder (\s -> (s { variantsBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> CreateExperimentInputBuilder ()
setDescription value =
   CreateExperimentInputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> CreateExperimentInputBuilder ()
setChangeReason value =
   CreateExperimentInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setMetrics :: Data.Maybe.Maybe Data.Aeson.Value -> CreateExperimentInputBuilder ()
setMetrics value =
   CreateExperimentInputBuilder (\s -> (s { metricsBuilderState = value }, ()))

build :: CreateExperimentInputBuilder () -> Data.Either.Either Data.Text.Text CreateExperimentInput
build builder = do
    let (st, _) = runCreateExperimentInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentInput.CreateExperimentInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentInput.CreateExperimentInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentInput.CreateExperimentInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    experiment_type' <- Data.Either.Right (experiment_typeBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentInput.CreateExperimentInput.context is a required property.") Data.Either.Right (contextBuilderState st)
    variants' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentInput.CreateExperimentInput.variants is a required property.") Data.Either.Right (variantsBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentInput.CreateExperimentInput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateExperimentInput.CreateExperimentInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    metrics' <- Data.Either.Right (metricsBuilderState st)
    Data.Either.Right (CreateExperimentInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        name = name',
        experiment_type = experiment_type',
        context = context',
        variants = variants',
        description = description',
        change_reason = change_reason',
        metrics = metrics'
    })



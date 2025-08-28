module Io.Superposition.Model.UpdateDimensionInput (
    setWorkspaceId,
    setOrgId,
    setDimension,
    setSchema,
    setPosition,
    setFunctionName,
    setDescription,
    setDependencies,
    setChangeReason,
    setAutocompleteFunctionName,
    setCohortBasedOn,
    build,
    UpdateDimensionInputBuilder,
    UpdateDimensionInput,
    workspace_id,
    org_id,
    dimension,
    schema,
    position,
    function_name,
    description,
    dependencies,
    change_reason,
    autocomplete_function_name,
    cohort_based_on
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

data UpdateDimensionInput = UpdateDimensionInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    dimension :: Data.Text.Text,
    schema :: Data.Maybe.Maybe Data.Aeson.Value,
    position :: Data.Maybe.Maybe Integer,
    function_name :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    dependencies :: Data.Maybe.Maybe ([] Data.Text.Text),
    change_reason :: Data.Text.Text,
    autocomplete_function_name :: Data.Maybe.Maybe Data.Text.Text,
    cohort_based_on :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateDimensionInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "dimension" Data.Aeson..= dimension a,
        "schema" Data.Aeson..= schema a,
        "position" Data.Aeson..= position a,
        "function_name" Data.Aeson..= function_name a,
        "description" Data.Aeson..= description a,
        "dependencies" Data.Aeson..= dependencies a,
        "change_reason" Data.Aeson..= change_reason a,
        "autocomplete_function_name" Data.Aeson..= autocomplete_function_name a,
        "cohort_based_on" Data.Aeson..= cohort_based_on a
        ]
    


instance Data.Aeson.FromJSON UpdateDimensionInput where
    parseJSON = Data.Aeson.withObject "UpdateDimensionInput" $ \v -> UpdateDimensionInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "dimension")
        Control.Applicative.<*> (v Data.Aeson..: "schema")
        Control.Applicative.<*> (v Data.Aeson..: "position")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "dependencies")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "autocomplete_function_name")
        Control.Applicative.<*> (v Data.Aeson..: "cohort_based_on")
    



data UpdateDimensionInputBuilderState = UpdateDimensionInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimensionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    schemaBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    positionBuilderState :: Data.Maybe.Maybe Integer,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dependenciesBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    autocomplete_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    cohort_based_onBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateDimensionInputBuilderState
defaultBuilderState = UpdateDimensionInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    dimensionBuilderState = Data.Maybe.Nothing,
    schemaBuilderState = Data.Maybe.Nothing,
    positionBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    dependenciesBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    autocomplete_function_nameBuilderState = Data.Maybe.Nothing,
    cohort_based_onBuilderState = Data.Maybe.Nothing
}

newtype UpdateDimensionInputBuilder a = UpdateDimensionInputBuilder {
    runUpdateDimensionInputBuilder :: UpdateDimensionInputBuilderState -> (UpdateDimensionInputBuilderState, a)
}

instance Data.Functor.Functor UpdateDimensionInputBuilder where
    fmap f (UpdateDimensionInputBuilder g) =
        UpdateDimensionInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateDimensionInputBuilder where
    pure a = UpdateDimensionInputBuilder (\s -> (s, a))
    (UpdateDimensionInputBuilder f) <*> (UpdateDimensionInputBuilder g) = UpdateDimensionInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateDimensionInputBuilder where
    (UpdateDimensionInputBuilder f) >>= g = UpdateDimensionInputBuilder (\s ->
        let (s', a) = f s
            (UpdateDimensionInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> UpdateDimensionInputBuilder ()
setWorkspaceId value =
   UpdateDimensionInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> UpdateDimensionInputBuilder ()
setOrgId value =
   UpdateDimensionInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setDimension :: Data.Text.Text -> UpdateDimensionInputBuilder ()
setDimension value =
   UpdateDimensionInputBuilder (\s -> (s { dimensionBuilderState = Data.Maybe.Just value }, ()))

setSchema :: Data.Maybe.Maybe Data.Aeson.Value -> UpdateDimensionInputBuilder ()
setSchema value =
   UpdateDimensionInputBuilder (\s -> (s { schemaBuilderState = value }, ()))

setPosition :: Data.Maybe.Maybe Integer -> UpdateDimensionInputBuilder ()
setPosition value =
   UpdateDimensionInputBuilder (\s -> (s { positionBuilderState = value }, ()))

setFunctionName :: Data.Maybe.Maybe Data.Text.Text -> UpdateDimensionInputBuilder ()
setFunctionName value =
   UpdateDimensionInputBuilder (\s -> (s { function_nameBuilderState = value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> UpdateDimensionInputBuilder ()
setDescription value =
   UpdateDimensionInputBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setDependencies :: Data.Maybe.Maybe ([] Data.Text.Text) -> UpdateDimensionInputBuilder ()
setDependencies value =
   UpdateDimensionInputBuilder (\s -> (s { dependenciesBuilderState = value }, ()))

setChangeReason :: Data.Text.Text -> UpdateDimensionInputBuilder ()
setChangeReason value =
   UpdateDimensionInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setAutocompleteFunctionName :: Data.Maybe.Maybe Data.Text.Text -> UpdateDimensionInputBuilder ()
setAutocompleteFunctionName value =
   UpdateDimensionInputBuilder (\s -> (s { autocomplete_function_nameBuilderState = value }, ()))

setCohortBasedOn :: Data.Maybe.Maybe Data.Text.Text -> UpdateDimensionInputBuilder ()
setCohortBasedOn value =
   UpdateDimensionInputBuilder (\s -> (s { cohort_based_onBuilderState = value }, ()))

build :: UpdateDimensionInputBuilder () -> Data.Either.Either Data.Text.Text UpdateDimensionInput
build builder = do
    let (st, _) = runUpdateDimensionInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDimensionInput.UpdateDimensionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDimensionInput.UpdateDimensionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    dimension' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDimensionInput.UpdateDimensionInput.dimension is a required property.") Data.Either.Right (dimensionBuilderState st)
    schema' <- Data.Either.Right (schemaBuilderState st)
    position' <- Data.Either.Right (positionBuilderState st)
    function_name' <- Data.Either.Right (function_nameBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    dependencies' <- Data.Either.Right (dependenciesBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDimensionInput.UpdateDimensionInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    autocomplete_function_name' <- Data.Either.Right (autocomplete_function_nameBuilderState st)
    cohort_based_on' <- Data.Either.Right (cohort_based_onBuilderState st)
    Data.Either.Right (UpdateDimensionInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        dimension = dimension',
        schema = schema',
        position = position',
        function_name = function_name',
        description = description',
        dependencies = dependencies',
        change_reason = change_reason',
        autocomplete_function_name = autocomplete_function_name',
        cohort_based_on = cohort_based_on'
    })



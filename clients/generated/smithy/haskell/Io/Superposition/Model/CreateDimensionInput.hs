module Io.Superposition.Model.CreateDimensionInput (
    setWorkspaceId,
    setOrgId,
    setDimension,
    setPosition,
    setSchema,
    setFunctionName,
    setDependencies,
    setDescription,
    setChangeReason,
    setAutocompleteFunctionName,
    setDimensionType,
    setCohortBasedOn,
    build,
    CreateDimensionInputBuilder,
    CreateDimensionInput,
    workspace_id,
    org_id,
    dimension,
    position,
    schema,
    function_name,
    dependencies,
    description,
    change_reason,
    autocomplete_function_name,
    dimension_type,
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
import qualified Io.Superposition.Model.DimensionType

data CreateDimensionInput = CreateDimensionInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    dimension :: Data.Text.Text,
    position :: Integer,
    schema :: Data.Aeson.Value,
    function_name :: Data.Maybe.Maybe Data.Text.Text,
    dependencies :: Data.Maybe.Maybe ([] Data.Text.Text),
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    autocomplete_function_name :: Data.Maybe.Maybe Data.Text.Text,
    dimension_type :: Io.Superposition.Model.DimensionType.DimensionType,
    cohort_based_on :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateDimensionInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "dimension" Data.Aeson..= dimension a,
        "position" Data.Aeson..= position a,
        "schema" Data.Aeson..= schema a,
        "function_name" Data.Aeson..= function_name a,
        "dependencies" Data.Aeson..= dependencies a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "autocomplete_function_name" Data.Aeson..= autocomplete_function_name a,
        "dimension_type" Data.Aeson..= dimension_type a,
        "cohort_based_on" Data.Aeson..= cohort_based_on a
        ]
    


instance Data.Aeson.FromJSON CreateDimensionInput where
    parseJSON = Data.Aeson.withObject "CreateDimensionInput" $ \v -> CreateDimensionInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "dimension")
        Control.Applicative.<*> (v Data.Aeson..: "position")
        Control.Applicative.<*> (v Data.Aeson..: "schema")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "dependencies")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "autocomplete_function_name")
        Control.Applicative.<*> (v Data.Aeson..: "dimension_type")
        Control.Applicative.<*> (v Data.Aeson..: "cohort_based_on")
    



data CreateDimensionInputBuilderState = CreateDimensionInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimensionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    positionBuilderState :: Data.Maybe.Maybe Integer,
    schemaBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dependenciesBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    autocomplete_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimension_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionType.DimensionType,
    cohort_based_onBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateDimensionInputBuilderState
defaultBuilderState = CreateDimensionInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    dimensionBuilderState = Data.Maybe.Nothing,
    positionBuilderState = Data.Maybe.Nothing,
    schemaBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing,
    dependenciesBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    autocomplete_function_nameBuilderState = Data.Maybe.Nothing,
    dimension_typeBuilderState = Data.Maybe.Nothing,
    cohort_based_onBuilderState = Data.Maybe.Nothing
}

newtype CreateDimensionInputBuilder a = CreateDimensionInputBuilder {
    runCreateDimensionInputBuilder :: CreateDimensionInputBuilderState -> (CreateDimensionInputBuilderState, a)
}

instance Data.Functor.Functor CreateDimensionInputBuilder where
    fmap f (CreateDimensionInputBuilder g) =
        CreateDimensionInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CreateDimensionInputBuilder where
    pure a = CreateDimensionInputBuilder (\s -> (s, a))
    (CreateDimensionInputBuilder f) <*> (CreateDimensionInputBuilder g) = CreateDimensionInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CreateDimensionInputBuilder where
    (CreateDimensionInputBuilder f) >>= g = CreateDimensionInputBuilder (\s ->
        let (s', a) = f s
            (CreateDimensionInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> CreateDimensionInputBuilder ()
setWorkspaceId value =
   CreateDimensionInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> CreateDimensionInputBuilder ()
setOrgId value =
   CreateDimensionInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setDimension :: Data.Text.Text -> CreateDimensionInputBuilder ()
setDimension value =
   CreateDimensionInputBuilder (\s -> (s { dimensionBuilderState = Data.Maybe.Just value }, ()))

setPosition :: Integer -> CreateDimensionInputBuilder ()
setPosition value =
   CreateDimensionInputBuilder (\s -> (s { positionBuilderState = Data.Maybe.Just value }, ()))

setSchema :: Data.Aeson.Value -> CreateDimensionInputBuilder ()
setSchema value =
   CreateDimensionInputBuilder (\s -> (s { schemaBuilderState = Data.Maybe.Just value }, ()))

setFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDimensionInputBuilder ()
setFunctionName value =
   CreateDimensionInputBuilder (\s -> (s { function_nameBuilderState = value }, ()))

setDependencies :: Data.Maybe.Maybe ([] Data.Text.Text) -> CreateDimensionInputBuilder ()
setDependencies value =
   CreateDimensionInputBuilder (\s -> (s { dependenciesBuilderState = value }, ()))

setDescription :: Data.Text.Text -> CreateDimensionInputBuilder ()
setDescription value =
   CreateDimensionInputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> CreateDimensionInputBuilder ()
setChangeReason value =
   CreateDimensionInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setAutocompleteFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDimensionInputBuilder ()
setAutocompleteFunctionName value =
   CreateDimensionInputBuilder (\s -> (s { autocomplete_function_nameBuilderState = value }, ()))

setDimensionType :: Io.Superposition.Model.DimensionType.DimensionType -> CreateDimensionInputBuilder ()
setDimensionType value =
   CreateDimensionInputBuilder (\s -> (s { dimension_typeBuilderState = Data.Maybe.Just value }, ()))

setCohortBasedOn :: Data.Maybe.Maybe Data.Text.Text -> CreateDimensionInputBuilder ()
setCohortBasedOn value =
   CreateDimensionInputBuilder (\s -> (s { cohort_based_onBuilderState = value }, ()))

build :: CreateDimensionInputBuilder () -> Data.Either.Either Data.Text.Text CreateDimensionInput
build builder = do
    let (st, _) = runCreateDimensionInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    dimension' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.dimension is a required property.") Data.Either.Right (dimensionBuilderState st)
    position' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.position is a required property.") Data.Either.Right (positionBuilderState st)
    schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.schema is a required property.") Data.Either.Right (schemaBuilderState st)
    function_name' <- Data.Either.Right (function_nameBuilderState st)
    dependencies' <- Data.Either.Right (dependenciesBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    autocomplete_function_name' <- Data.Either.Right (autocomplete_function_nameBuilderState st)
    dimension_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.dimension_type is a required property.") Data.Either.Right (dimension_typeBuilderState st)
    cohort_based_on' <- Data.Either.Right (cohort_based_onBuilderState st)
    Data.Either.Right (CreateDimensionInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        dimension = dimension',
        position = position',
        schema = schema',
        function_name = function_name',
        dependencies = dependencies',
        description = description',
        change_reason = change_reason',
        autocomplete_function_name = autocomplete_function_name',
        dimension_type = dimension_type',
        cohort_based_on = cohort_based_on'
    })



module Io.Superposition.Model.CreateDimensionOutput (
    setDimension,
    setPosition,
    setSchema,
    setFunctionName,
    setDescription,
    setChangeReason,
    setLastModifiedAt,
    setLastModifiedBy,
    setCreatedAt,
    setCreatedBy,
    setDependencies,
    setDependents,
    setDependencyGraph,
    setAutocompleteFunctionName,
    setDimensionType,
    setCohortBasedOn,
    setMandatory,
    build,
    CreateDimensionOutputBuilder,
    CreateDimensionOutput,
    dimension,
    position,
    schema,
    function_name,
    description,
    change_reason,
    last_modified_at,
    last_modified_by,
    created_at,
    created_by,
    dependencies,
    dependents,
    dependency_graph,
    autocomplete_function_name,
    dimension_type,
    cohort_based_on,
    mandatory
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
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.DimensionType

data CreateDimensionOutput = CreateDimensionOutput {
    dimension :: Data.Text.Text,
    position :: Integer,
    schema :: Data.Aeson.Value,
    function_name :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    created_by :: Data.Text.Text,
    dependencies :: [] Data.Text.Text,
    dependents :: [] Data.Text.Text,
    dependency_graph :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    autocomplete_function_name :: Data.Maybe.Maybe Data.Text.Text,
    dimension_type :: Io.Superposition.Model.DimensionType.DimensionType,
    cohort_based_on :: Data.Maybe.Maybe Data.Text.Text,
    mandatory :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateDimensionOutput where
    toJSON a = Data.Aeson.object [
        "dimension" Data.Aeson..= dimension a,
        "position" Data.Aeson..= position a,
        "schema" Data.Aeson..= schema a,
        "function_name" Data.Aeson..= function_name a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "last_modified_at" Data.Aeson..= last_modified_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "created_at" Data.Aeson..= created_at a,
        "created_by" Data.Aeson..= created_by a,
        "dependencies" Data.Aeson..= dependencies a,
        "dependents" Data.Aeson..= dependents a,
        "dependency_graph" Data.Aeson..= dependency_graph a,
        "autocomplete_function_name" Data.Aeson..= autocomplete_function_name a,
        "dimension_type" Data.Aeson..= dimension_type a,
        "cohort_based_on" Data.Aeson..= cohort_based_on a,
        "mandatory" Data.Aeson..= mandatory a
        ]
    


instance Data.Aeson.FromJSON CreateDimensionOutput where
    parseJSON = Data.Aeson.withObject "CreateDimensionOutput" $ \v -> CreateDimensionOutput
        Data.Functor.<$> (v Data.Aeson..: "dimension")
        Control.Applicative.<*> (v Data.Aeson..: "position")
        Control.Applicative.<*> (v Data.Aeson..: "schema")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "dependencies")
        Control.Applicative.<*> (v Data.Aeson..: "dependents")
        Control.Applicative.<*> (v Data.Aeson..: "dependency_graph")
        Control.Applicative.<*> (v Data.Aeson..: "autocomplete_function_name")
        Control.Applicative.<*> (v Data.Aeson..: "dimension_type")
        Control.Applicative.<*> (v Data.Aeson..: "cohort_based_on")
        Control.Applicative.<*> (v Data.Aeson..: "mandatory")
    



data CreateDimensionOutputBuilderState = CreateDimensionOutputBuilderState {
    dimensionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    positionBuilderState :: Data.Maybe.Maybe Integer,
    schemaBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dependenciesBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    dependentsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    dependency_graphBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    autocomplete_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimension_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionType.DimensionType,
    cohort_based_onBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    mandatoryBuilderState :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateDimensionOutputBuilderState
defaultBuilderState = CreateDimensionOutputBuilderState {
    dimensionBuilderState = Data.Maybe.Nothing,
    positionBuilderState = Data.Maybe.Nothing,
    schemaBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    dependenciesBuilderState = Data.Maybe.Nothing,
    dependentsBuilderState = Data.Maybe.Nothing,
    dependency_graphBuilderState = Data.Maybe.Nothing,
    autocomplete_function_nameBuilderState = Data.Maybe.Nothing,
    dimension_typeBuilderState = Data.Maybe.Nothing,
    cohort_based_onBuilderState = Data.Maybe.Nothing,
    mandatoryBuilderState = Data.Maybe.Nothing
}

newtype CreateDimensionOutputBuilder a = CreateDimensionOutputBuilder {
    runCreateDimensionOutputBuilder :: CreateDimensionOutputBuilderState -> (CreateDimensionOutputBuilderState, a)
}

instance Data.Functor.Functor CreateDimensionOutputBuilder where
    fmap f (CreateDimensionOutputBuilder g) =
        CreateDimensionOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CreateDimensionOutputBuilder where
    pure a = CreateDimensionOutputBuilder (\s -> (s, a))
    (CreateDimensionOutputBuilder f) <*> (CreateDimensionOutputBuilder g) = CreateDimensionOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CreateDimensionOutputBuilder where
    (CreateDimensionOutputBuilder f) >>= g = CreateDimensionOutputBuilder (\s ->
        let (s', a) = f s
            (CreateDimensionOutputBuilder h) = g a
        in h s')

setDimension :: Data.Text.Text -> CreateDimensionOutputBuilder ()
setDimension value =
   CreateDimensionOutputBuilder (\s -> (s { dimensionBuilderState = Data.Maybe.Just value }, ()))

setPosition :: Integer -> CreateDimensionOutputBuilder ()
setPosition value =
   CreateDimensionOutputBuilder (\s -> (s { positionBuilderState = Data.Maybe.Just value }, ()))

setSchema :: Data.Aeson.Value -> CreateDimensionOutputBuilder ()
setSchema value =
   CreateDimensionOutputBuilder (\s -> (s { schemaBuilderState = Data.Maybe.Just value }, ()))

setFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDimensionOutputBuilder ()
setFunctionName value =
   CreateDimensionOutputBuilder (\s -> (s { function_nameBuilderState = value }, ()))

setDescription :: Data.Text.Text -> CreateDimensionOutputBuilder ()
setDescription value =
   CreateDimensionOutputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> CreateDimensionOutputBuilder ()
setChangeReason value =
   CreateDimensionOutputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedAt :: Data.Time.UTCTime -> CreateDimensionOutputBuilder ()
setLastModifiedAt value =
   CreateDimensionOutputBuilder (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> CreateDimensionOutputBuilder ()
setLastModifiedBy value =
   CreateDimensionOutputBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

setCreatedAt :: Data.Time.UTCTime -> CreateDimensionOutputBuilder ()
setCreatedAt value =
   CreateDimensionOutputBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setCreatedBy :: Data.Text.Text -> CreateDimensionOutputBuilder ()
setCreatedBy value =
   CreateDimensionOutputBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setDependencies :: [] Data.Text.Text -> CreateDimensionOutputBuilder ()
setDependencies value =
   CreateDimensionOutputBuilder (\s -> (s { dependenciesBuilderState = Data.Maybe.Just value }, ()))

setDependents :: [] Data.Text.Text -> CreateDimensionOutputBuilder ()
setDependents value =
   CreateDimensionOutputBuilder (\s -> (s { dependentsBuilderState = Data.Maybe.Just value }, ()))

setDependencyGraph :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> CreateDimensionOutputBuilder ()
setDependencyGraph value =
   CreateDimensionOutputBuilder (\s -> (s { dependency_graphBuilderState = Data.Maybe.Just value }, ()))

setAutocompleteFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDimensionOutputBuilder ()
setAutocompleteFunctionName value =
   CreateDimensionOutputBuilder (\s -> (s { autocomplete_function_nameBuilderState = value }, ()))

setDimensionType :: Io.Superposition.Model.DimensionType.DimensionType -> CreateDimensionOutputBuilder ()
setDimensionType value =
   CreateDimensionOutputBuilder (\s -> (s { dimension_typeBuilderState = Data.Maybe.Just value }, ()))

setCohortBasedOn :: Data.Maybe.Maybe Data.Text.Text -> CreateDimensionOutputBuilder ()
setCohortBasedOn value =
   CreateDimensionOutputBuilder (\s -> (s { cohort_based_onBuilderState = value }, ()))

setMandatory :: Data.Maybe.Maybe Bool -> CreateDimensionOutputBuilder ()
setMandatory value =
   CreateDimensionOutputBuilder (\s -> (s { mandatoryBuilderState = value }, ()))

build :: CreateDimensionOutputBuilder () -> Data.Either.Either Data.Text.Text CreateDimensionOutput
build builder = do
    let (st, _) = runCreateDimensionOutputBuilder builder defaultBuilderState
    dimension' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.dimension is a required property.") Data.Either.Right (dimensionBuilderState st)
    position' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.position is a required property.") Data.Either.Right (positionBuilderState st)
    schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.schema is a required property.") Data.Either.Right (schemaBuilderState st)
    function_name' <- Data.Either.Right (function_nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    dependencies' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.dependencies is a required property.") Data.Either.Right (dependenciesBuilderState st)
    dependents' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.dependents is a required property.") Data.Either.Right (dependentsBuilderState st)
    dependency_graph' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.dependency_graph is a required property.") Data.Either.Right (dependency_graphBuilderState st)
    autocomplete_function_name' <- Data.Either.Right (autocomplete_function_nameBuilderState st)
    dimension_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.dimension_type is a required property.") Data.Either.Right (dimension_typeBuilderState st)
    cohort_based_on' <- Data.Either.Right (cohort_based_onBuilderState st)
    mandatory' <- Data.Either.Right (mandatoryBuilderState st)
    Data.Either.Right (CreateDimensionOutput { 
        dimension = dimension',
        position = position',
        schema = schema',
        function_name = function_name',
        description = description',
        change_reason = change_reason',
        last_modified_at = last_modified_at',
        last_modified_by = last_modified_by',
        created_at = created_at',
        created_by = created_by',
        dependencies = dependencies',
        dependents = dependents',
        dependency_graph = dependency_graph',
        autocomplete_function_name = autocomplete_function_name',
        dimension_type = dimension_type',
        cohort_based_on = cohort_based_on',
        mandatory = mandatory'
    })



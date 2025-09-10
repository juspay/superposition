module Io.Superposition.Model.DimensionExt (
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
    setMandatory,
    build,
    DimensionExtBuilder,
    DimensionExt,
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
    mandatory
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
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

data DimensionExt = DimensionExt {
    dimension :: Data.Text.Text,
    position :: Data.Int.Int32,
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
    mandatory :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DimensionExt where
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
        "mandatory" Data.Aeson..= mandatory a
        ]
    

instance Io.Superposition.Utility.SerializeBody DimensionExt

instance Data.Aeson.FromJSON DimensionExt where
    parseJSON = Data.Aeson.withObject "DimensionExt" $ \v -> DimensionExt
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
        Control.Applicative.<*> (v Data.Aeson..: "mandatory")
    



data DimensionExtBuilderState = DimensionExtBuilderState {
    dimensionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    positionBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
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
    mandatoryBuilderState :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DimensionExtBuilderState
defaultBuilderState = DimensionExtBuilderState {
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
    mandatoryBuilderState = Data.Maybe.Nothing
}

type DimensionExtBuilder = Control.Monad.State.Strict.State DimensionExtBuilderState

setDimension :: Data.Text.Text -> DimensionExtBuilder ()
setDimension value =
   Control.Monad.State.Strict.modify (\s -> (s { dimensionBuilderState = Data.Maybe.Just value }))

setPosition :: Data.Int.Int32 -> DimensionExtBuilder ()
setPosition value =
   Control.Monad.State.Strict.modify (\s -> (s { positionBuilderState = Data.Maybe.Just value }))

setSchema :: Data.Aeson.Value -> DimensionExtBuilder ()
setSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { schemaBuilderState = Data.Maybe.Just value }))

setFunctionName :: Data.Maybe.Maybe Data.Text.Text -> DimensionExtBuilder ()
setFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { function_nameBuilderState = value }))

setDescription :: Data.Text.Text -> DimensionExtBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> DimensionExtBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> DimensionExtBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> DimensionExtBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> DimensionExtBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> DimensionExtBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setDependencies :: [] Data.Text.Text -> DimensionExtBuilder ()
setDependencies value =
   Control.Monad.State.Strict.modify (\s -> (s { dependenciesBuilderState = Data.Maybe.Just value }))

setDependents :: [] Data.Text.Text -> DimensionExtBuilder ()
setDependents value =
   Control.Monad.State.Strict.modify (\s -> (s { dependentsBuilderState = Data.Maybe.Just value }))

setDependencyGraph :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> DimensionExtBuilder ()
setDependencyGraph value =
   Control.Monad.State.Strict.modify (\s -> (s { dependency_graphBuilderState = Data.Maybe.Just value }))

setAutocompleteFunctionName :: Data.Maybe.Maybe Data.Text.Text -> DimensionExtBuilder ()
setAutocompleteFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { autocomplete_function_nameBuilderState = value }))

setMandatory :: Data.Maybe.Maybe Bool -> DimensionExtBuilder ()
setMandatory value =
   Control.Monad.State.Strict.modify (\s -> (s { mandatoryBuilderState = value }))

build :: DimensionExtBuilder () -> Data.Either.Either Data.Text.Text DimensionExt
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    dimension' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.dimension is a required property.") Data.Either.Right (dimensionBuilderState st)
    position' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.position is a required property.") Data.Either.Right (positionBuilderState st)
    schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.schema is a required property.") Data.Either.Right (schemaBuilderState st)
    function_name' <- Data.Either.Right (function_nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    dependencies' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.dependencies is a required property.") Data.Either.Right (dependenciesBuilderState st)
    dependents' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.dependents is a required property.") Data.Either.Right (dependentsBuilderState st)
    dependency_graph' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionExt.DimensionExt.dependency_graph is a required property.") Data.Either.Right (dependency_graphBuilderState st)
    autocomplete_function_name' <- Data.Either.Right (autocomplete_function_nameBuilderState st)
    mandatory' <- Data.Either.Right (mandatoryBuilderState st)
    Data.Either.Right (DimensionExt { 
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
        mandatory = mandatory'
    })



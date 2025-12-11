module Io.Superposition.Model.DimensionResponse (
    setDimension,
    setPosition,
    setSchema,
    setValueValidationFunctionName,
    setDescription,
    setChangeReason,
    setLastModifiedAt,
    setLastModifiedBy,
    setCreatedAt,
    setCreatedBy,
    setDependencyGraph,
    setDimensionType,
    setValueComputeFunctionName,
    setMandatory,
    build,
    DimensionResponseBuilder,
    DimensionResponse,
    dimension,
    position,
    schema,
    value_validation_function_name,
    description,
    change_reason,
    last_modified_at,
    last_modified_by,
    created_at,
    created_by,
    dependency_graph,
    dimension_type,
    value_compute_function_name,
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
import qualified Io.Superposition.Model.DimensionType
import qualified Io.Superposition.Utility

data DimensionResponse = DimensionResponse {
    dimension :: Data.Text.Text,
    position :: Data.Int.Int32,
    schema :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    value_validation_function_name :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    created_by :: Data.Text.Text,
    dependency_graph :: Data.Map.Map Data.Text.Text ([] Data.Text.Text),
    dimension_type :: Io.Superposition.Model.DimensionType.DimensionType,
    value_compute_function_name :: Data.Maybe.Maybe Data.Text.Text,
    mandatory :: Bool
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DimensionResponse where
    toJSON a = Data.Aeson.object [
        "dimension" Data.Aeson..= dimension a,
        "position" Data.Aeson..= position a,
        "schema" Data.Aeson..= schema a,
        "value_validation_function_name" Data.Aeson..= value_validation_function_name a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "last_modified_at" Data.Aeson..= last_modified_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "created_at" Data.Aeson..= created_at a,
        "created_by" Data.Aeson..= created_by a,
        "dependency_graph" Data.Aeson..= dependency_graph a,
        "dimension_type" Data.Aeson..= dimension_type a,
        "value_compute_function_name" Data.Aeson..= value_compute_function_name a,
        "mandatory" Data.Aeson..= mandatory a
        ]
    

instance Io.Superposition.Utility.SerializeBody DimensionResponse

instance Data.Aeson.FromJSON DimensionResponse where
    parseJSON = Data.Aeson.withObject "DimensionResponse" $ \v -> DimensionResponse
        Data.Functor.<$> (v Data.Aeson..: "dimension")
        Control.Applicative.<*> (v Data.Aeson..: "position")
        Control.Applicative.<*> (v Data.Aeson..: "schema")
        Control.Applicative.<*> (v Data.Aeson..:? "value_validation_function_name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "dependency_graph")
        Control.Applicative.<*> (v Data.Aeson..: "dimension_type")
        Control.Applicative.<*> (v Data.Aeson..:? "value_compute_function_name")
        Control.Applicative.<*> (v Data.Aeson..: "mandatory")
    



data DimensionResponseBuilderState = DimensionResponseBuilderState {
    dimensionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    positionBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    schemaBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    value_validation_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dependency_graphBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text ([] Data.Text.Text)),
    dimension_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionType.DimensionType,
    value_compute_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    mandatoryBuilderState :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DimensionResponseBuilderState
defaultBuilderState = DimensionResponseBuilderState {
    dimensionBuilderState = Data.Maybe.Nothing,
    positionBuilderState = Data.Maybe.Nothing,
    schemaBuilderState = Data.Maybe.Nothing,
    value_validation_function_nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    dependency_graphBuilderState = Data.Maybe.Nothing,
    dimension_typeBuilderState = Data.Maybe.Nothing,
    value_compute_function_nameBuilderState = Data.Maybe.Nothing,
    mandatoryBuilderState = Data.Maybe.Nothing
}

type DimensionResponseBuilder = Control.Monad.State.Strict.State DimensionResponseBuilderState

setDimension :: Data.Text.Text -> DimensionResponseBuilder ()
setDimension value =
   Control.Monad.State.Strict.modify (\s -> (s { dimensionBuilderState = Data.Maybe.Just value }))

setPosition :: Data.Int.Int32 -> DimensionResponseBuilder ()
setPosition value =
   Control.Monad.State.Strict.modify (\s -> (s { positionBuilderState = Data.Maybe.Just value }))

setSchema :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> DimensionResponseBuilder ()
setSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { schemaBuilderState = Data.Maybe.Just value }))

setValueValidationFunctionName :: Data.Maybe.Maybe Data.Text.Text -> DimensionResponseBuilder ()
setValueValidationFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { value_validation_function_nameBuilderState = value }))

setDescription :: Data.Text.Text -> DimensionResponseBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> DimensionResponseBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> DimensionResponseBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> DimensionResponseBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> DimensionResponseBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> DimensionResponseBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setDependencyGraph :: Data.Map.Map Data.Text.Text ([] Data.Text.Text) -> DimensionResponseBuilder ()
setDependencyGraph value =
   Control.Monad.State.Strict.modify (\s -> (s { dependency_graphBuilderState = Data.Maybe.Just value }))

setDimensionType :: Io.Superposition.Model.DimensionType.DimensionType -> DimensionResponseBuilder ()
setDimensionType value =
   Control.Monad.State.Strict.modify (\s -> (s { dimension_typeBuilderState = Data.Maybe.Just value }))

setValueComputeFunctionName :: Data.Maybe.Maybe Data.Text.Text -> DimensionResponseBuilder ()
setValueComputeFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { value_compute_function_nameBuilderState = value }))

setMandatory :: Bool -> DimensionResponseBuilder ()
setMandatory value =
   Control.Monad.State.Strict.modify (\s -> (s { mandatoryBuilderState = Data.Maybe.Just value }))

build :: DimensionResponseBuilder () -> Data.Either.Either Data.Text.Text DimensionResponse
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    dimension' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.dimension is a required property.") Data.Either.Right (dimensionBuilderState st)
    position' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.position is a required property.") Data.Either.Right (positionBuilderState st)
    schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.schema is a required property.") Data.Either.Right (schemaBuilderState st)
    value_validation_function_name' <- Data.Either.Right (value_validation_function_nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    dependency_graph' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.dependency_graph is a required property.") Data.Either.Right (dependency_graphBuilderState st)
    dimension_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.dimension_type is a required property.") Data.Either.Right (dimension_typeBuilderState st)
    value_compute_function_name' <- Data.Either.Right (value_compute_function_nameBuilderState st)
    mandatory' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DimensionResponse.DimensionResponse.mandatory is a required property.") Data.Either.Right (mandatoryBuilderState st)
    Data.Either.Right (DimensionResponse { 
        dimension = dimension',
        position = position',
        schema = schema',
        value_validation_function_name = value_validation_function_name',
        description = description',
        change_reason = change_reason',
        last_modified_at = last_modified_at',
        last_modified_by = last_modified_by',
        created_at = created_at',
        created_by = created_by',
        dependency_graph = dependency_graph',
        dimension_type = dimension_type',
        value_compute_function_name = value_compute_function_name',
        mandatory = mandatory'
    })



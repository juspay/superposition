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
    setDependencyGraph,
    setDimensionType,
    setAutocompleteFunctionName,
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
    dependency_graph,
    dimension_type,
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
import qualified Io.Superposition.Model.DimensionType
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data CreateDimensionOutput = CreateDimensionOutput {
    dimension :: Data.Text.Text,
    position :: Data.Int.Int32,
    schema :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    function_name :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    created_by :: Data.Text.Text,
    dependency_graph :: Data.Map.Map Data.Text.Text ([] Data.Text.Text),
    dimension_type :: Io.Superposition.Model.DimensionType.DimensionType,
    autocomplete_function_name :: Data.Maybe.Maybe Data.Text.Text,
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
        "dependency_graph" Data.Aeson..= dependency_graph a,
        "dimension_type" Data.Aeson..= dimension_type a,
        "autocomplete_function_name" Data.Aeson..= autocomplete_function_name a,
        "mandatory" Data.Aeson..= mandatory a
        ]
    

instance Io.Superposition.Utility.SerializeBody CreateDimensionOutput

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
        Control.Applicative.<*> (v Data.Aeson..: "dependency_graph")
        Control.Applicative.<*> (v Data.Aeson..: "dimension_type")
        Control.Applicative.<*> (v Data.Aeson..: "autocomplete_function_name")
        Control.Applicative.<*> (v Data.Aeson..: "mandatory")
    



data CreateDimensionOutputBuilderState = CreateDimensionOutputBuilderState {
    dimensionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    positionBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    schemaBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dependency_graphBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text ([] Data.Text.Text)),
    dimension_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionType.DimensionType,
    autocomplete_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
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
    dependency_graphBuilderState = Data.Maybe.Nothing,
    dimension_typeBuilderState = Data.Maybe.Nothing,
    autocomplete_function_nameBuilderState = Data.Maybe.Nothing,
    mandatoryBuilderState = Data.Maybe.Nothing
}

type CreateDimensionOutputBuilder = Control.Monad.State.Strict.State CreateDimensionOutputBuilderState

setDimension :: Data.Text.Text -> CreateDimensionOutputBuilder ()
setDimension value =
   Control.Monad.State.Strict.modify (\s -> (s { dimensionBuilderState = Data.Maybe.Just value }))

setPosition :: Data.Int.Int32 -> CreateDimensionOutputBuilder ()
setPosition value =
   Control.Monad.State.Strict.modify (\s -> (s { positionBuilderState = Data.Maybe.Just value }))

setSchema :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> CreateDimensionOutputBuilder ()
setSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { schemaBuilderState = Data.Maybe.Just value }))

setFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDimensionOutputBuilder ()
setFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { function_nameBuilderState = value }))

setDescription :: Data.Text.Text -> CreateDimensionOutputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> CreateDimensionOutputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> CreateDimensionOutputBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> CreateDimensionOutputBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> CreateDimensionOutputBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> CreateDimensionOutputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setDependencyGraph :: Data.Map.Map Data.Text.Text ([] Data.Text.Text) -> CreateDimensionOutputBuilder ()
setDependencyGraph value =
   Control.Monad.State.Strict.modify (\s -> (s { dependency_graphBuilderState = Data.Maybe.Just value }))

setDimensionType :: Io.Superposition.Model.DimensionType.DimensionType -> CreateDimensionOutputBuilder ()
setDimensionType value =
   Control.Monad.State.Strict.modify (\s -> (s { dimension_typeBuilderState = Data.Maybe.Just value }))

setAutocompleteFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDimensionOutputBuilder ()
setAutocompleteFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { autocomplete_function_nameBuilderState = value }))

setMandatory :: Data.Maybe.Maybe Bool -> CreateDimensionOutputBuilder ()
setMandatory value =
   Control.Monad.State.Strict.modify (\s -> (s { mandatoryBuilderState = value }))

build :: CreateDimensionOutputBuilder () -> Data.Either.Either Data.Text.Text CreateDimensionOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
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
    dependency_graph' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.dependency_graph is a required property.") Data.Either.Right (dependency_graphBuilderState st)
    dimension_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionOutput.CreateDimensionOutput.dimension_type is a required property.") Data.Either.Right (dimension_typeBuilderState st)
    autocomplete_function_name' <- Data.Either.Right (autocomplete_function_nameBuilderState st)
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
        dependency_graph = dependency_graph',
        dimension_type = dimension_type',
        autocomplete_function_name = autocomplete_function_name',
        mandatory = mandatory'
    })


instance Io.Superposition.Utility.FromResponseParser CreateDimensionOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "schema"
        var1 <- Io.Superposition.Utility.deSerField "description"
        var2 <- Io.Superposition.Utility.deSerField "created_at"
        var3 <- Io.Superposition.Utility.deSerField "last_modified_by"
        var4 <- Io.Superposition.Utility.deSerField "created_by"
        var5 <- Io.Superposition.Utility.deSerField "mandatory"
        var6 <- Io.Superposition.Utility.deSerField "last_modified_at"
        var7 <- Io.Superposition.Utility.deSerField "dependency_graph"
        var8 <- Io.Superposition.Utility.deSerField "autocomplete_function_name"
        var9 <- Io.Superposition.Utility.deSerField "change_reason"
        var10 <- Io.Superposition.Utility.deSerField "dimension_type"
        var11 <- Io.Superposition.Utility.deSerField "function_name"
        var12 <- Io.Superposition.Utility.deSerField "position"
        var13 <- Io.Superposition.Utility.deSerField "dimension"
        pure $ CreateDimensionOutput {
            dimension = var13,
            position = var12,
            schema = var0,
            function_name = var11,
            description = var1,
            change_reason = var9,
            last_modified_at = var6,
            last_modified_by = var3,
            created_at = var2,
            created_by = var4,
            dependency_graph = var7,
            dimension_type = var10,
            autocomplete_function_name = var8,
            mandatory = var5
        }


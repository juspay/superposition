module Io.Superposition.Model.CreateDimensionInput (
    setWorkspaceId,
    setOrgId,
    setDimension,
    setPosition,
    setSchema,
    setValueValidationFunctionName,
    setDescription,
    setChangeReason,
    setDimensionType,
    setValueComputeFunctionName,
    build,
    CreateDimensionInputBuilder,
    CreateDimensionInput,
    workspace_id,
    org_id,
    dimension,
    position,
    schema,
    value_validation_function_name,
    description,
    change_reason,
    dimension_type,
    value_compute_function_name
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
import qualified Io.Superposition.Model.DimensionType
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data CreateDimensionInput = CreateDimensionInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    dimension :: Data.Text.Text,
    position :: Data.Int.Int32,
    schema :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    value_validation_function_name :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    dimension_type :: Data.Maybe.Maybe Io.Superposition.Model.DimensionType.DimensionType,
    value_compute_function_name :: Data.Maybe.Maybe Data.Text.Text
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
        "value_validation_function_name" Data.Aeson..= value_validation_function_name a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "dimension_type" Data.Aeson..= dimension_type a,
        "value_compute_function_name" Data.Aeson..= value_compute_function_name a
        ]
    

instance Io.Superposition.Utility.SerializeBody CreateDimensionInput

instance Data.Aeson.FromJSON CreateDimensionInput where
    parseJSON = Data.Aeson.withObject "CreateDimensionInput" $ \v -> CreateDimensionInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "dimension")
        Control.Applicative.<*> (v Data.Aeson..: "position")
        Control.Applicative.<*> (v Data.Aeson..: "schema")
        Control.Applicative.<*> (v Data.Aeson..:? "value_validation_function_name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..:? "dimension_type")
        Control.Applicative.<*> (v Data.Aeson..:? "value_compute_function_name")
    



data CreateDimensionInputBuilderState = CreateDimensionInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimensionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    positionBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    schemaBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    value_validation_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimension_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionType.DimensionType,
    value_compute_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
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
    value_validation_function_nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    dimension_typeBuilderState = Data.Maybe.Nothing,
    value_compute_function_nameBuilderState = Data.Maybe.Nothing
}

type CreateDimensionInputBuilder = Control.Monad.State.Strict.State CreateDimensionInputBuilderState

setWorkspaceId :: Data.Text.Text -> CreateDimensionInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> CreateDimensionInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setDimension :: Data.Text.Text -> CreateDimensionInputBuilder ()
setDimension value =
   Control.Monad.State.Strict.modify (\s -> (s { dimensionBuilderState = Data.Maybe.Just value }))

setPosition :: Data.Int.Int32 -> CreateDimensionInputBuilder ()
setPosition value =
   Control.Monad.State.Strict.modify (\s -> (s { positionBuilderState = Data.Maybe.Just value }))

setSchema :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> CreateDimensionInputBuilder ()
setSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { schemaBuilderState = Data.Maybe.Just value }))

setValueValidationFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDimensionInputBuilder ()
setValueValidationFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { value_validation_function_nameBuilderState = value }))

setDescription :: Data.Text.Text -> CreateDimensionInputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> CreateDimensionInputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setDimensionType :: Data.Maybe.Maybe Io.Superposition.Model.DimensionType.DimensionType -> CreateDimensionInputBuilder ()
setDimensionType value =
   Control.Monad.State.Strict.modify (\s -> (s { dimension_typeBuilderState = value }))

setValueComputeFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDimensionInputBuilder ()
setValueComputeFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { value_compute_function_nameBuilderState = value }))

build :: CreateDimensionInputBuilder () -> Data.Either.Either Data.Text.Text CreateDimensionInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    dimension' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.dimension is a required property.") Data.Either.Right (dimensionBuilderState st)
    position' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.position is a required property.") Data.Either.Right (positionBuilderState st)
    schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.schema is a required property.") Data.Either.Right (schemaBuilderState st)
    value_validation_function_name' <- Data.Either.Right (value_validation_function_nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDimensionInput.CreateDimensionInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    dimension_type' <- Data.Either.Right (dimension_typeBuilderState st)
    value_compute_function_name' <- Data.Either.Right (value_compute_function_nameBuilderState st)
    Data.Either.Right (CreateDimensionInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        dimension = dimension',
        position = position',
        schema = schema',
        value_validation_function_name = value_validation_function_name',
        description = description',
        change_reason = change_reason',
        dimension_type = dimension_type',
        value_compute_function_name = value_compute_function_name'
    })


instance Io.Superposition.Utility.IntoRequestBuilder CreateDimensionInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "dimension"
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "schema" (schema self)
        Io.Superposition.Utility.serField "change_reason" (change_reason self)
        Io.Superposition.Utility.serField "dimension_type" (dimension_type self)
        Io.Superposition.Utility.serField "value_compute_function_name" (value_compute_function_name self)
        Io.Superposition.Utility.serField "description" (description self)
        Io.Superposition.Utility.serField "position" (position self)
        Io.Superposition.Utility.serField "dimension" (dimension self)
        Io.Superposition.Utility.serField "value_validation_function_name" (value_validation_function_name self)


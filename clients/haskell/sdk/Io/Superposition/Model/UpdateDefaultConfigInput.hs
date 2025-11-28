module Io.Superposition.Model.UpdateDefaultConfigInput (
    setWorkspaceId,
    setOrgId,
    setKey,
    setChangeReason,
    setValue,
    setSchema,
    setValueValidationFunctionName,
    setDescription,
    setValueComputeFunctionName,
    build,
    UpdateDefaultConfigInputBuilder,
    UpdateDefaultConfigInput,
    workspace_id,
    org_id,
    key,
    change_reason,
    value,
    schema,
    value_validation_function_name,
    description,
    value_compute_function_name
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data UpdateDefaultConfigInput = UpdateDefaultConfigInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    key :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    value :: Data.Maybe.Maybe Data.Aeson.Value,
    schema :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    value_validation_function_name :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    value_compute_function_name :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateDefaultConfigInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "key" Data.Aeson..= key a,
        "change_reason" Data.Aeson..= change_reason a,
        "value" Data.Aeson..= value a,
        "schema" Data.Aeson..= schema a,
        "value_validation_function_name" Data.Aeson..= value_validation_function_name a,
        "description" Data.Aeson..= description a,
        "value_compute_function_name" Data.Aeson..= value_compute_function_name a
        ]
    

instance Io.Superposition.Utility.SerializeBody UpdateDefaultConfigInput

instance Data.Aeson.FromJSON UpdateDefaultConfigInput where
    parseJSON = Data.Aeson.withObject "UpdateDefaultConfigInput" $ \v -> UpdateDefaultConfigInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "key")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..:? "value")
        Control.Applicative.<*> (v Data.Aeson..:? "schema")
        Control.Applicative.<*> (v Data.Aeson..:? "value_validation_function_name")
        Control.Applicative.<*> (v Data.Aeson..:? "description")
        Control.Applicative.<*> (v Data.Aeson..:? "value_compute_function_name")
    



data UpdateDefaultConfigInputBuilderState = UpdateDefaultConfigInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    valueBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    schemaBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    value_validation_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    value_compute_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateDefaultConfigInputBuilderState
defaultBuilderState = UpdateDefaultConfigInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    keyBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    valueBuilderState = Data.Maybe.Nothing,
    schemaBuilderState = Data.Maybe.Nothing,
    value_validation_function_nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    value_compute_function_nameBuilderState = Data.Maybe.Nothing
}

type UpdateDefaultConfigInputBuilder = Control.Monad.State.Strict.State UpdateDefaultConfigInputBuilderState

setWorkspaceId :: Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setKey :: Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setKey value =
   Control.Monad.State.Strict.modify (\s -> (s { keyBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setValue :: Data.Maybe.Maybe Data.Aeson.Value -> UpdateDefaultConfigInputBuilder ()
setValue value =
   Control.Monad.State.Strict.modify (\s -> (s { valueBuilderState = value }))

setSchema :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> UpdateDefaultConfigInputBuilder ()
setSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { schemaBuilderState = value }))

setValueValidationFunctionName :: Data.Maybe.Maybe Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setValueValidationFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { value_validation_function_nameBuilderState = value }))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = value }))

setValueComputeFunctionName :: Data.Maybe.Maybe Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setValueComputeFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { value_compute_function_nameBuilderState = value }))

build :: UpdateDefaultConfigInputBuilder () -> Data.Either.Either Data.Text.Text UpdateDefaultConfigInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput.key is a required property.") Data.Either.Right (keyBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    value' <- Data.Either.Right (valueBuilderState st)
    schema' <- Data.Either.Right (schemaBuilderState st)
    value_validation_function_name' <- Data.Either.Right (value_validation_function_nameBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    value_compute_function_name' <- Data.Either.Right (value_compute_function_nameBuilderState st)
    Data.Either.Right (UpdateDefaultConfigInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        key = key',
        change_reason = change_reason',
        value = value',
        schema = schema',
        value_validation_function_name = value_validation_function_name',
        description = description',
        value_compute_function_name = value_compute_function_name'
    })


instance Io.Superposition.Utility.IntoRequestBuilder UpdateDefaultConfigInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPatch
        Io.Superposition.Utility.setPath [
            "default-config",
            Io.Superposition.Utility.serializeElement (key self)
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "schema" (schema self)
        Io.Superposition.Utility.serField "change_reason" (change_reason self)
        Io.Superposition.Utility.serField "value_compute_function_name" (value_compute_function_name self)
        Io.Superposition.Utility.serField "description" (description self)
        Io.Superposition.Utility.serField "value" (value self)
        Io.Superposition.Utility.serField "value_validation_function_name" (value_validation_function_name self)


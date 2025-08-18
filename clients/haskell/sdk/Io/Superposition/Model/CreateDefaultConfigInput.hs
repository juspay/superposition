module Io.Superposition.Model.CreateDefaultConfigInput (
    setKey,
    setValue,
    setSchema,
    setDescription,
    setChangeReason,
    setFunctionName,
    setAutocompleteFunctionName,
    setWorkspaceId,
    setOrgId,
    build,
    CreateDefaultConfigInputBuilder,
    CreateDefaultConfigInput,
    key,
    value,
    schema,
    description,
    change_reason,
    function_name,
    autocomplete_function_name,
    workspace_id,
    org_id
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data CreateDefaultConfigInput = CreateDefaultConfigInput {
    key :: Data.Text.Text,
    value :: Data.Aeson.Value,
    schema :: Data.Aeson.Value,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    function_name :: Data.Maybe.Maybe Data.Text.Text,
    autocomplete_function_name :: Data.Maybe.Maybe Data.Text.Text,
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateDefaultConfigInput where
    toJSON a = Data.Aeson.object [
        "key" Data.Aeson..= key a,
        "value" Data.Aeson..= value a,
        "schema" Data.Aeson..= schema a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "function_name" Data.Aeson..= function_name a,
        "autocomplete_function_name" Data.Aeson..= autocomplete_function_name a,
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a
        ]
    

instance Io.Superposition.Utility.SerializeBody CreateDefaultConfigInput

instance Data.Aeson.FromJSON CreateDefaultConfigInput where
    parseJSON = Data.Aeson.withObject "CreateDefaultConfigInput" $ \v -> CreateDefaultConfigInput
        Data.Functor.<$> (v Data.Aeson..: "key")
        Control.Applicative.<*> (v Data.Aeson..: "value")
        Control.Applicative.<*> (v Data.Aeson..: "schema")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "autocomplete_function_name")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
    



data CreateDefaultConfigInputBuilderState = CreateDefaultConfigInputBuilderState {
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    valueBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    schemaBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    autocomplete_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateDefaultConfigInputBuilderState
defaultBuilderState = CreateDefaultConfigInputBuilderState {
    keyBuilderState = Data.Maybe.Nothing,
    valueBuilderState = Data.Maybe.Nothing,
    schemaBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing,
    autocomplete_function_nameBuilderState = Data.Maybe.Nothing,
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing
}

type CreateDefaultConfigInputBuilder = Control.Monad.State.Strict.State CreateDefaultConfigInputBuilderState

setKey :: Data.Text.Text -> CreateDefaultConfigInputBuilder ()
setKey value =
   Control.Monad.State.Strict.modify (\s -> (s { keyBuilderState = Data.Maybe.Just value }))

setValue :: Data.Aeson.Value -> CreateDefaultConfigInputBuilder ()
setValue value =
   Control.Monad.State.Strict.modify (\s -> (s { valueBuilderState = Data.Maybe.Just value }))

setSchema :: Data.Aeson.Value -> CreateDefaultConfigInputBuilder ()
setSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { schemaBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> CreateDefaultConfigInputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> CreateDefaultConfigInputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDefaultConfigInputBuilder ()
setFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { function_nameBuilderState = value }))

setAutocompleteFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDefaultConfigInputBuilder ()
setAutocompleteFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { autocomplete_function_nameBuilderState = value }))

setWorkspaceId :: Data.Text.Text -> CreateDefaultConfigInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> CreateDefaultConfigInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

build :: CreateDefaultConfigInputBuilder () -> Data.Either.Either Data.Text.Text CreateDefaultConfigInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigInput.CreateDefaultConfigInput.key is a required property.") Data.Either.Right (keyBuilderState st)
    value' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigInput.CreateDefaultConfigInput.value is a required property.") Data.Either.Right (valueBuilderState st)
    schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigInput.CreateDefaultConfigInput.schema is a required property.") Data.Either.Right (schemaBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigInput.CreateDefaultConfigInput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigInput.CreateDefaultConfigInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    function_name' <- Data.Either.Right (function_nameBuilderState st)
    autocomplete_function_name' <- Data.Either.Right (autocomplete_function_nameBuilderState st)
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigInput.CreateDefaultConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigInput.CreateDefaultConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    Data.Either.Right (CreateDefaultConfigInput { 
        key = key',
        value = value',
        schema = schema',
        description = description',
        change_reason = change_reason',
        function_name = function_name',
        autocomplete_function_name = autocomplete_function_name',
        workspace_id = workspace_id',
        org_id = org_id'
    })


instance Io.Superposition.Utility.IntoRequestBuilder CreateDefaultConfigInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "default-config"
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "schema" (schema self)
        Io.Superposition.Utility.serField "autocomplete_function_name" (autocomplete_function_name self)
        Io.Superposition.Utility.serField "change_reason" (change_reason self)
        Io.Superposition.Utility.serField "function_name" (function_name self)
        Io.Superposition.Utility.serField "description" (description self)
        Io.Superposition.Utility.serField "value" (value self)
        Io.Superposition.Utility.serField "key" (key self)


module Io.Superposition.Model.UpdateDefaultConfigInput (
    setWorkspaceId,
    setOrgId,
    setKey,
    setChangeReason,
    setValue,
    setSchema,
    setFunctionName,
    setDescription,
    setAutocompleteFunctionName,
    build,
    UpdateDefaultConfigInputBuilder,
    UpdateDefaultConfigInput,
    workspace_id,
    org_id,
    key,
    change_reason,
    value,
    schema,
    function_name,
    description,
    autocomplete_function_name
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

data UpdateDefaultConfigInput = UpdateDefaultConfigInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    key :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    value :: Data.Maybe.Maybe Data.Aeson.Value,
    schema :: Data.Maybe.Maybe Data.Aeson.Value,
    function_name :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    autocomplete_function_name :: Data.Maybe.Maybe Data.Text.Text
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
        "function_name" Data.Aeson..= function_name a,
        "description" Data.Aeson..= description a,
        "autocomplete_function_name" Data.Aeson..= autocomplete_function_name a
        ]
    


instance Data.Aeson.FromJSON UpdateDefaultConfigInput where
    parseJSON = Data.Aeson.withObject "UpdateDefaultConfigInput" $ \v -> UpdateDefaultConfigInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "key")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "value")
        Control.Applicative.<*> (v Data.Aeson..: "schema")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "autocomplete_function_name")
    



data UpdateDefaultConfigInputBuilderState = UpdateDefaultConfigInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    valueBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    schemaBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    autocomplete_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
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
    function_nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    autocomplete_function_nameBuilderState = Data.Maybe.Nothing
}

newtype UpdateDefaultConfigInputBuilder a = UpdateDefaultConfigInputBuilder {
    runUpdateDefaultConfigInputBuilder :: UpdateDefaultConfigInputBuilderState -> (UpdateDefaultConfigInputBuilderState, a)
}

instance Data.Functor.Functor UpdateDefaultConfigInputBuilder where
    fmap f (UpdateDefaultConfigInputBuilder g) =
        UpdateDefaultConfigInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateDefaultConfigInputBuilder where
    pure a = UpdateDefaultConfigInputBuilder (\s -> (s, a))
    (UpdateDefaultConfigInputBuilder f) <*> (UpdateDefaultConfigInputBuilder g) = UpdateDefaultConfigInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateDefaultConfigInputBuilder where
    (UpdateDefaultConfigInputBuilder f) >>= g = UpdateDefaultConfigInputBuilder (\s ->
        let (s', a) = f s
            (UpdateDefaultConfigInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setWorkspaceId value =
   UpdateDefaultConfigInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setOrgId value =
   UpdateDefaultConfigInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setKey :: Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setKey value =
   UpdateDefaultConfigInputBuilder (\s -> (s { keyBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setChangeReason value =
   UpdateDefaultConfigInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setValue :: Data.Maybe.Maybe Data.Aeson.Value -> UpdateDefaultConfigInputBuilder ()
setValue value =
   UpdateDefaultConfigInputBuilder (\s -> (s { valueBuilderState = value }, ()))

setSchema :: Data.Maybe.Maybe Data.Aeson.Value -> UpdateDefaultConfigInputBuilder ()
setSchema value =
   UpdateDefaultConfigInputBuilder (\s -> (s { schemaBuilderState = value }, ()))

setFunctionName :: Data.Maybe.Maybe Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setFunctionName value =
   UpdateDefaultConfigInputBuilder (\s -> (s { function_nameBuilderState = value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setDescription value =
   UpdateDefaultConfigInputBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setAutocompleteFunctionName :: Data.Maybe.Maybe Data.Text.Text -> UpdateDefaultConfigInputBuilder ()
setAutocompleteFunctionName value =
   UpdateDefaultConfigInputBuilder (\s -> (s { autocomplete_function_nameBuilderState = value }, ()))

build :: UpdateDefaultConfigInputBuilder () -> Data.Either.Either Data.Text.Text UpdateDefaultConfigInput
build builder = do
    let (st, _) = runUpdateDefaultConfigInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput.key is a required property.") Data.Either.Right (keyBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    value' <- Data.Either.Right (valueBuilderState st)
    schema' <- Data.Either.Right (schemaBuilderState st)
    function_name' <- Data.Either.Right (function_nameBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    autocomplete_function_name' <- Data.Either.Right (autocomplete_function_nameBuilderState st)
    Data.Either.Right (UpdateDefaultConfigInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        key = key',
        change_reason = change_reason',
        value = value',
        schema = schema',
        function_name = function_name',
        description = description',
        autocomplete_function_name = autocomplete_function_name'
    })



module Io.Superposition.Model.CreateFunctionInput (
    setWorkspaceId,
    setOrgId,
    setFunctionName,
    setDescription,
    setChangeReason,
    setFunction,
    setRuntimeVersion,
    setFunctionType,
    build,
    CreateFunctionInputBuilder,
    CreateFunctionInput,
    workspace_id,
    org_id,
    function_name,
    description,
    change_reason,
    function,
    runtime_version,
    function_type
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
import qualified Io.Superposition.Model.FunctionTypes

data CreateFunctionInput = CreateFunctionInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    function_name :: Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    function :: Data.Text.Text,
    runtime_version :: Data.Text.Text,
    function_type :: Io.Superposition.Model.FunctionTypes.FunctionTypes
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateFunctionInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "function_name" Data.Aeson..= function_name a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "function" Data.Aeson..= function a,
        "runtime_version" Data.Aeson..= runtime_version a,
        "function_type" Data.Aeson..= function_type a
        ]
    


instance Data.Aeson.FromJSON CreateFunctionInput where
    parseJSON = Data.Aeson.withObject "CreateFunctionInput" $ \v -> CreateFunctionInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "function")
        Control.Applicative.<*> (v Data.Aeson..: "runtime_version")
        Control.Applicative.<*> (v Data.Aeson..: "function_type")
    



data CreateFunctionInputBuilderState = CreateFunctionInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    functionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    runtime_versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.FunctionTypes.FunctionTypes
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateFunctionInputBuilderState
defaultBuilderState = CreateFunctionInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    functionBuilderState = Data.Maybe.Nothing,
    runtime_versionBuilderState = Data.Maybe.Nothing,
    function_typeBuilderState = Data.Maybe.Nothing
}

newtype CreateFunctionInputBuilder a = CreateFunctionInputBuilder {
    runCreateFunctionInputBuilder :: CreateFunctionInputBuilderState -> (CreateFunctionInputBuilderState, a)
}

instance Data.Functor.Functor CreateFunctionInputBuilder where
    fmap f (CreateFunctionInputBuilder g) =
        CreateFunctionInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CreateFunctionInputBuilder where
    pure a = CreateFunctionInputBuilder (\s -> (s, a))
    (CreateFunctionInputBuilder f) <*> (CreateFunctionInputBuilder g) = CreateFunctionInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CreateFunctionInputBuilder where
    (CreateFunctionInputBuilder f) >>= g = CreateFunctionInputBuilder (\s ->
        let (s', a) = f s
            (CreateFunctionInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> CreateFunctionInputBuilder ()
setWorkspaceId value =
   CreateFunctionInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> CreateFunctionInputBuilder ()
setOrgId value =
   CreateFunctionInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setFunctionName :: Data.Text.Text -> CreateFunctionInputBuilder ()
setFunctionName value =
   CreateFunctionInputBuilder (\s -> (s { function_nameBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> CreateFunctionInputBuilder ()
setDescription value =
   CreateFunctionInputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> CreateFunctionInputBuilder ()
setChangeReason value =
   CreateFunctionInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setFunction :: Data.Text.Text -> CreateFunctionInputBuilder ()
setFunction value =
   CreateFunctionInputBuilder (\s -> (s { functionBuilderState = Data.Maybe.Just value }, ()))

setRuntimeVersion :: Data.Text.Text -> CreateFunctionInputBuilder ()
setRuntimeVersion value =
   CreateFunctionInputBuilder (\s -> (s { runtime_versionBuilderState = Data.Maybe.Just value }, ()))

setFunctionType :: Io.Superposition.Model.FunctionTypes.FunctionTypes -> CreateFunctionInputBuilder ()
setFunctionType value =
   CreateFunctionInputBuilder (\s -> (s { function_typeBuilderState = Data.Maybe.Just value }, ()))

build :: CreateFunctionInputBuilder () -> Data.Either.Either Data.Text.Text CreateFunctionInput
build builder = do
    let (st, _) = runCreateFunctionInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    function_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput.function_name is a required property.") Data.Either.Right (function_nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    function' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput.function is a required property.") Data.Either.Right (functionBuilderState st)
    runtime_version' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput.runtime_version is a required property.") Data.Either.Right (runtime_versionBuilderState st)
    function_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput.function_type is a required property.") Data.Either.Right (function_typeBuilderState st)
    Data.Either.Right (CreateFunctionInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        function_name = function_name',
        description = description',
        change_reason = change_reason',
        function = function',
        runtime_version = runtime_version',
        function_type = function_type'
    })



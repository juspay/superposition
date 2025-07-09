module Io.Superposition.Model.UpdateFunctionInput (
    setWorkspaceId,
    setOrgId,
    setFunctionName,
    setDescription,
    setChangeReason,
    setFunction,
    setRuntimeVersion,
    build,
    UpdateFunctionInputBuilder,
    UpdateFunctionInput,
    workspace_id,
    org_id,
    function_name,
    description,
    change_reason,
    function,
    runtime_version
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

data UpdateFunctionInput = UpdateFunctionInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    function_name :: Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Text.Text,
    function :: Data.Text.Text,
    runtime_version :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateFunctionInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "function_name" Data.Aeson..= function_name a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "function" Data.Aeson..= function a,
        "runtime_version" Data.Aeson..= runtime_version a
        ]
    


instance Data.Aeson.FromJSON UpdateFunctionInput where
    parseJSON = Data.Aeson.withObject "UpdateFunctionInput" $ \v -> UpdateFunctionInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "function")
        Control.Applicative.<*> (v Data.Aeson..: "runtime_version")
    



data UpdateFunctionInputBuilderState = UpdateFunctionInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    functionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    runtime_versionBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateFunctionInputBuilderState
defaultBuilderState = UpdateFunctionInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    functionBuilderState = Data.Maybe.Nothing,
    runtime_versionBuilderState = Data.Maybe.Nothing
}

newtype UpdateFunctionInputBuilder a = UpdateFunctionInputBuilder {
    runUpdateFunctionInputBuilder :: UpdateFunctionInputBuilderState -> (UpdateFunctionInputBuilderState, a)
}

instance Data.Functor.Functor UpdateFunctionInputBuilder where
    fmap f (UpdateFunctionInputBuilder g) =
        UpdateFunctionInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateFunctionInputBuilder where
    pure a = UpdateFunctionInputBuilder (\s -> (s, a))
    (UpdateFunctionInputBuilder f) <*> (UpdateFunctionInputBuilder g) = UpdateFunctionInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateFunctionInputBuilder where
    (UpdateFunctionInputBuilder f) >>= g = UpdateFunctionInputBuilder (\s ->
        let (s', a) = f s
            (UpdateFunctionInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> UpdateFunctionInputBuilder ()
setWorkspaceId value =
   UpdateFunctionInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> UpdateFunctionInputBuilder ()
setOrgId value =
   UpdateFunctionInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setFunctionName :: Data.Text.Text -> UpdateFunctionInputBuilder ()
setFunctionName value =
   UpdateFunctionInputBuilder (\s -> (s { function_nameBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> UpdateFunctionInputBuilder ()
setDescription value =
   UpdateFunctionInputBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setChangeReason :: Data.Text.Text -> UpdateFunctionInputBuilder ()
setChangeReason value =
   UpdateFunctionInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setFunction :: Data.Text.Text -> UpdateFunctionInputBuilder ()
setFunction value =
   UpdateFunctionInputBuilder (\s -> (s { functionBuilderState = Data.Maybe.Just value }, ()))

setRuntimeVersion :: Data.Text.Text -> UpdateFunctionInputBuilder ()
setRuntimeVersion value =
   UpdateFunctionInputBuilder (\s -> (s { runtime_versionBuilderState = Data.Maybe.Just value }, ()))

build :: UpdateFunctionInputBuilder () -> Data.Either.Either Data.Text.Text UpdateFunctionInput
build builder = do
    let (st, _) = runUpdateFunctionInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionInput.UpdateFunctionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionInput.UpdateFunctionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    function_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionInput.UpdateFunctionInput.function_name is a required property.") Data.Either.Right (function_nameBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionInput.UpdateFunctionInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    function' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionInput.UpdateFunctionInput.function is a required property.") Data.Either.Right (functionBuilderState st)
    runtime_version' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionInput.UpdateFunctionInput.runtime_version is a required property.") Data.Either.Right (runtime_versionBuilderState st)
    Data.Either.Right (UpdateFunctionInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        function_name = function_name',
        description = description',
        change_reason = change_reason',
        function = function',
        runtime_version = runtime_version'
    })



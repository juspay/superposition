module Io.Superposition.Model.DeleteFunctionInput (
    setWorkspaceId,
    setOrgId,
    setFunctionName,
    build,
    DeleteFunctionInputBuilder,
    DeleteFunctionInput,
    workspace_id,
    org_id,
    function_name
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

data DeleteFunctionInput = DeleteFunctionInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    function_name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteFunctionInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "function_name" Data.Aeson..= function_name a
        ]
    


instance Data.Aeson.FromJSON DeleteFunctionInput where
    parseJSON = Data.Aeson.withObject "DeleteFunctionInput" $ \v -> DeleteFunctionInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
    



data DeleteFunctionInputBuilderState = DeleteFunctionInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteFunctionInputBuilderState
defaultBuilderState = DeleteFunctionInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing
}

newtype DeleteFunctionInputBuilder a = DeleteFunctionInputBuilder {
    runDeleteFunctionInputBuilder :: DeleteFunctionInputBuilderState -> (DeleteFunctionInputBuilderState, a)
}

instance Data.Functor.Functor DeleteFunctionInputBuilder where
    fmap f (DeleteFunctionInputBuilder g) =
        DeleteFunctionInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DeleteFunctionInputBuilder where
    pure a = DeleteFunctionInputBuilder (\s -> (s, a))
    (DeleteFunctionInputBuilder f) <*> (DeleteFunctionInputBuilder g) = DeleteFunctionInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DeleteFunctionInputBuilder where
    (DeleteFunctionInputBuilder f) >>= g = DeleteFunctionInputBuilder (\s ->
        let (s', a) = f s
            (DeleteFunctionInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> DeleteFunctionInputBuilder ()
setWorkspaceId value =
   DeleteFunctionInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> DeleteFunctionInputBuilder ()
setOrgId value =
   DeleteFunctionInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setFunctionName :: Data.Text.Text -> DeleteFunctionInputBuilder ()
setFunctionName value =
   DeleteFunctionInputBuilder (\s -> (s { function_nameBuilderState = Data.Maybe.Just value }, ()))

build :: DeleteFunctionInputBuilder () -> Data.Either.Either Data.Text.Text DeleteFunctionInput
build builder = do
    let (st, _) = runDeleteFunctionInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteFunctionInput.DeleteFunctionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteFunctionInput.DeleteFunctionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    function_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteFunctionInput.DeleteFunctionInput.function_name is a required property.") Data.Either.Right (function_nameBuilderState st)
    Data.Either.Right (DeleteFunctionInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        function_name = function_name'
    })



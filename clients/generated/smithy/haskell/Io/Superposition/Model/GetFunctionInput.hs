module Io.Superposition.Model.GetFunctionInput (
    setWorkspaceId,
    setOrgId,
    setFunctionName,
    build,
    GetFunctionInputBuilder,
    GetFunctionInput,
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

data GetFunctionInput = GetFunctionInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    function_name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetFunctionInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "function_name" Data.Aeson..= function_name a
        ]
    


instance Data.Aeson.FromJSON GetFunctionInput where
    parseJSON = Data.Aeson.withObject "GetFunctionInput" $ \v -> GetFunctionInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
    



data GetFunctionInputBuilderState = GetFunctionInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetFunctionInputBuilderState
defaultBuilderState = GetFunctionInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing
}

newtype GetFunctionInputBuilder a = GetFunctionInputBuilder {
    runGetFunctionInputBuilder :: GetFunctionInputBuilderState -> (GetFunctionInputBuilderState, a)
}

instance Data.Functor.Functor GetFunctionInputBuilder where
    fmap f (GetFunctionInputBuilder g) =
        GetFunctionInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetFunctionInputBuilder where
    pure a = GetFunctionInputBuilder (\s -> (s, a))
    (GetFunctionInputBuilder f) <*> (GetFunctionInputBuilder g) = GetFunctionInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetFunctionInputBuilder where
    (GetFunctionInputBuilder f) >>= g = GetFunctionInputBuilder (\s ->
        let (s', a) = f s
            (GetFunctionInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> GetFunctionInputBuilder ()
setWorkspaceId value =
   GetFunctionInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> GetFunctionInputBuilder ()
setOrgId value =
   GetFunctionInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setFunctionName :: Data.Text.Text -> GetFunctionInputBuilder ()
setFunctionName value =
   GetFunctionInputBuilder (\s -> (s { function_nameBuilderState = Data.Maybe.Just value }, ()))

build :: GetFunctionInputBuilder () -> Data.Either.Either Data.Text.Text GetFunctionInput
build builder = do
    let (st, _) = runGetFunctionInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetFunctionInput.GetFunctionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetFunctionInput.GetFunctionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    function_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetFunctionInput.GetFunctionInput.function_name is a required property.") Data.Either.Right (function_nameBuilderState st)
    Data.Either.Right (GetFunctionInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        function_name = function_name'
    })



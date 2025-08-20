module Io.Superposition.Model.GetContextFromConditionInput (
    setWorkspaceId,
    setOrgId,
    setContext,
    build,
    GetContextFromConditionInputBuilder,
    GetContextFromConditionInput,
    workspace_id,
    org_id,
    context
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

data GetContextFromConditionInput = GetContextFromConditionInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    context :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetContextFromConditionInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "context" Data.Aeson..= context a
        ]
    


instance Data.Aeson.FromJSON GetContextFromConditionInput where
    parseJSON = Data.Aeson.withObject "GetContextFromConditionInput" $ \v -> GetContextFromConditionInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "context")
    



data GetContextFromConditionInputBuilderState = GetContextFromConditionInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetContextFromConditionInputBuilderState
defaultBuilderState = GetContextFromConditionInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing
}

newtype GetContextFromConditionInputBuilder a = GetContextFromConditionInputBuilder {
    runGetContextFromConditionInputBuilder :: GetContextFromConditionInputBuilderState -> (GetContextFromConditionInputBuilderState, a)
}

instance Data.Functor.Functor GetContextFromConditionInputBuilder where
    fmap f (GetContextFromConditionInputBuilder g) =
        GetContextFromConditionInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetContextFromConditionInputBuilder where
    pure a = GetContextFromConditionInputBuilder (\s -> (s, a))
    (GetContextFromConditionInputBuilder f) <*> (GetContextFromConditionInputBuilder g) = GetContextFromConditionInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetContextFromConditionInputBuilder where
    (GetContextFromConditionInputBuilder f) >>= g = GetContextFromConditionInputBuilder (\s ->
        let (s', a) = f s
            (GetContextFromConditionInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> GetContextFromConditionInputBuilder ()
setWorkspaceId value =
   GetContextFromConditionInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> GetContextFromConditionInputBuilder ()
setOrgId value =
   GetContextFromConditionInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setContext :: Data.Maybe.Maybe Data.Aeson.Value -> GetContextFromConditionInputBuilder ()
setContext value =
   GetContextFromConditionInputBuilder (\s -> (s { contextBuilderState = value }, ()))

build :: GetContextFromConditionInputBuilder () -> Data.Either.Either Data.Text.Text GetContextFromConditionInput
build builder = do
    let (st, _) = runGetContextFromConditionInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetContextFromConditionInput.GetContextFromConditionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetContextFromConditionInput.GetContextFromConditionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    context' <- Data.Either.Right (contextBuilderState st)
    Data.Either.Right (GetContextFromConditionInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        context = context'
    })



module Io.Superposition.Model.GetContextInput (
    setWorkspaceId,
    setOrgId,
    setId',
    build,
    GetContextInputBuilder,
    GetContextInput,
    workspace_id,
    org_id,
    id'
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

data GetContextInput = GetContextInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetContextInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a
        ]
    


instance Data.Aeson.FromJSON GetContextInput where
    parseJSON = Data.Aeson.withObject "GetContextInput" $ \v -> GetContextInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
    



data GetContextInputBuilderState = GetContextInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetContextInputBuilderState
defaultBuilderState = GetContextInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing
}

newtype GetContextInputBuilder a = GetContextInputBuilder {
    runGetContextInputBuilder :: GetContextInputBuilderState -> (GetContextInputBuilderState, a)
}

instance Data.Functor.Functor GetContextInputBuilder where
    fmap f (GetContextInputBuilder g) =
        GetContextInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetContextInputBuilder where
    pure a = GetContextInputBuilder (\s -> (s, a))
    (GetContextInputBuilder f) <*> (GetContextInputBuilder g) = GetContextInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetContextInputBuilder where
    (GetContextInputBuilder f) >>= g = GetContextInputBuilder (\s ->
        let (s', a) = f s
            (GetContextInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> GetContextInputBuilder ()
setWorkspaceId value =
   GetContextInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> GetContextInputBuilder ()
setOrgId value =
   GetContextInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setId' :: Data.Text.Text -> GetContextInputBuilder ()
setId' value =
   GetContextInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

build :: GetContextInputBuilder () -> Data.Either.Either Data.Text.Text GetContextInput
build builder = do
    let (st, _) = runGetContextInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetContextInput.GetContextInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetContextInput.GetContextInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetContextInput.GetContextInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    Data.Either.Right (GetContextInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id''
    })



module Io.Superposition.Model.MoveContextInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setContext,
    setDescription,
    setChangeReason,
    build,
    MoveContextInputBuilder,
    MoveContextInput,
    workspace_id,
    org_id,
    id',
    context,
    description,
    change_reason
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data MoveContextInput = MoveContextInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON MoveContextInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "context" Data.Aeson..= context a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON MoveContextInput where
    parseJSON = Data.Aeson.withObject "MoveContextInput" $ \v -> MoveContextInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data MoveContextInputBuilderState = MoveContextInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: MoveContextInputBuilderState
defaultBuilderState = MoveContextInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype MoveContextInputBuilder a = MoveContextInputBuilder {
    runMoveContextInputBuilder :: MoveContextInputBuilderState -> (MoveContextInputBuilderState, a)
}

instance Data.Functor.Functor MoveContextInputBuilder where
    fmap f (MoveContextInputBuilder g) =
        MoveContextInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative MoveContextInputBuilder where
    pure a = MoveContextInputBuilder (\s -> (s, a))
    (MoveContextInputBuilder f) <*> (MoveContextInputBuilder g) = MoveContextInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad MoveContextInputBuilder where
    (MoveContextInputBuilder f) >>= g = MoveContextInputBuilder (\s ->
        let (s', a) = f s
            (MoveContextInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> MoveContextInputBuilder ()
setWorkspaceId value =
   MoveContextInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> MoveContextInputBuilder ()
setOrgId value =
   MoveContextInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setId' :: Data.Text.Text -> MoveContextInputBuilder ()
setId' value =
   MoveContextInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> MoveContextInputBuilder ()
setContext value =
   MoveContextInputBuilder (\s -> (s { contextBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> MoveContextInputBuilder ()
setDescription value =
   MoveContextInputBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setChangeReason :: Data.Text.Text -> MoveContextInputBuilder ()
setChangeReason value =
   MoveContextInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: MoveContextInputBuilder () -> Data.Either.Either Data.Text.Text MoveContextInput
build builder = do
    let (st, _) = runMoveContextInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextInput.MoveContextInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextInput.MoveContextInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextInput.MoveContextInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextInput.MoveContextInput.context is a required property.") Data.Either.Right (contextBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextInput.MoveContextInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (MoveContextInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        context = context',
        description = description',
        change_reason = change_reason'
    })



module Io.Superposition.Model.DeleteContextInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setConfigTags,
    build,
    DeleteContextInputBuilder,
    DeleteContextInput,
    workspace_id,
    org_id,
    id',
    config_tags
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

data DeleteContextInput = DeleteContextInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    config_tags :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteContextInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "config_tags" Data.Aeson..= config_tags a
        ]
    


instance Data.Aeson.FromJSON DeleteContextInput where
    parseJSON = Data.Aeson.withObject "DeleteContextInput" $ \v -> DeleteContextInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "config_tags")
    



data DeleteContextInputBuilderState = DeleteContextInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    config_tagsBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteContextInputBuilderState
defaultBuilderState = DeleteContextInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    config_tagsBuilderState = Data.Maybe.Nothing
}

newtype DeleteContextInputBuilder a = DeleteContextInputBuilder {
    runDeleteContextInputBuilder :: DeleteContextInputBuilderState -> (DeleteContextInputBuilderState, a)
}

instance Data.Functor.Functor DeleteContextInputBuilder where
    fmap f (DeleteContextInputBuilder g) =
        DeleteContextInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DeleteContextInputBuilder where
    pure a = DeleteContextInputBuilder (\s -> (s, a))
    (DeleteContextInputBuilder f) <*> (DeleteContextInputBuilder g) = DeleteContextInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DeleteContextInputBuilder where
    (DeleteContextInputBuilder f) >>= g = DeleteContextInputBuilder (\s ->
        let (s', a) = f s
            (DeleteContextInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> DeleteContextInputBuilder ()
setWorkspaceId value =
   DeleteContextInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> DeleteContextInputBuilder ()
setOrgId value =
   DeleteContextInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setId' :: Data.Text.Text -> DeleteContextInputBuilder ()
setId' value =
   DeleteContextInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setConfigTags :: Data.Maybe.Maybe Data.Text.Text -> DeleteContextInputBuilder ()
setConfigTags value =
   DeleteContextInputBuilder (\s -> (s { config_tagsBuilderState = value }, ()))

build :: DeleteContextInputBuilder () -> Data.Either.Either Data.Text.Text DeleteContextInput
build builder = do
    let (st, _) = runDeleteContextInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteContextInput.DeleteContextInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteContextInput.DeleteContextInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteContextInput.DeleteContextInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    config_tags' <- Data.Either.Right (config_tagsBuilderState st)
    Data.Either.Right (DeleteContextInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        config_tags = config_tags'
    })



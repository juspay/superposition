module Io.Superposition.Model.CreateContextInput (
    setWorkspaceId,
    setOrgId,
    setContext,
    setConfigTags,
    setOverride,
    setDescription,
    setChangeReason,
    build,
    CreateContextInputBuilder,
    CreateContextInput,
    workspace_id,
    org_id,
    context,
    config_tags,
    override,
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

data CreateContextInput = CreateContextInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    config_tags :: Data.Maybe.Maybe Data.Text.Text,
    override :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateContextInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "context" Data.Aeson..= context a,
        "config_tags" Data.Aeson..= config_tags a,
        "override" Data.Aeson..= override a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON CreateContextInput where
    parseJSON = Data.Aeson.withObject "CreateContextInput" $ \v -> CreateContextInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "config_tags")
        Control.Applicative.<*> (v Data.Aeson..: "override")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data CreateContextInputBuilderState = CreateContextInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    config_tagsBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    overrideBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateContextInputBuilderState
defaultBuilderState = CreateContextInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing,
    config_tagsBuilderState = Data.Maybe.Nothing,
    overrideBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype CreateContextInputBuilder a = CreateContextInputBuilder {
    runCreateContextInputBuilder :: CreateContextInputBuilderState -> (CreateContextInputBuilderState, a)
}

instance Data.Functor.Functor CreateContextInputBuilder where
    fmap f (CreateContextInputBuilder g) =
        CreateContextInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CreateContextInputBuilder where
    pure a = CreateContextInputBuilder (\s -> (s, a))
    (CreateContextInputBuilder f) <*> (CreateContextInputBuilder g) = CreateContextInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CreateContextInputBuilder where
    (CreateContextInputBuilder f) >>= g = CreateContextInputBuilder (\s ->
        let (s', a) = f s
            (CreateContextInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> CreateContextInputBuilder ()
setWorkspaceId value =
   CreateContextInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> CreateContextInputBuilder ()
setOrgId value =
   CreateContextInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> CreateContextInputBuilder ()
setContext value =
   CreateContextInputBuilder (\s -> (s { contextBuilderState = Data.Maybe.Just value }, ()))

setConfigTags :: Data.Maybe.Maybe Data.Text.Text -> CreateContextInputBuilder ()
setConfigTags value =
   CreateContextInputBuilder (\s -> (s { config_tagsBuilderState = value }, ()))

setOverride :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> CreateContextInputBuilder ()
setOverride value =
   CreateContextInputBuilder (\s -> (s { overrideBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> CreateContextInputBuilder ()
setDescription value =
   CreateContextInputBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setChangeReason :: Data.Text.Text -> CreateContextInputBuilder ()
setChangeReason value =
   CreateContextInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: CreateContextInputBuilder () -> Data.Either.Either Data.Text.Text CreateContextInput
build builder = do
    let (st, _) = runCreateContextInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextInput.CreateContextInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextInput.CreateContextInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextInput.CreateContextInput.context is a required property.") Data.Either.Right (contextBuilderState st)
    config_tags' <- Data.Either.Right (config_tagsBuilderState st)
    override' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextInput.CreateContextInput.override is a required property.") Data.Either.Right (overrideBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextInput.CreateContextInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (CreateContextInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        context = context',
        config_tags = config_tags',
        override = override',
        description = description',
        change_reason = change_reason'
    })



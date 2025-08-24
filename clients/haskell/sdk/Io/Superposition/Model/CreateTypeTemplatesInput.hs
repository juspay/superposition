module Io.Superposition.Model.CreateTypeTemplatesInput (
    setWorkspaceId,
    setOrgId,
    setTypeName,
    setTypeSchema,
    setDescription,
    setChangeReason,
    build,
    CreateTypeTemplatesInputBuilder,
    CreateTypeTemplatesInput,
    workspace_id,
    org_id,
    type_name,
    type_schema,
    description,
    change_reason
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

data CreateTypeTemplatesInput = CreateTypeTemplatesInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    type_name :: Data.Text.Text,
    type_schema :: Data.Aeson.Value,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateTypeTemplatesInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "type_name" Data.Aeson..= type_name a,
        "type_schema" Data.Aeson..= type_schema a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON CreateTypeTemplatesInput where
    parseJSON = Data.Aeson.withObject "CreateTypeTemplatesInput" $ \v -> CreateTypeTemplatesInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "type_name")
        Control.Applicative.<*> (v Data.Aeson..: "type_schema")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data CreateTypeTemplatesInputBuilderState = CreateTypeTemplatesInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    type_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    type_schemaBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateTypeTemplatesInputBuilderState
defaultBuilderState = CreateTypeTemplatesInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    type_nameBuilderState = Data.Maybe.Nothing,
    type_schemaBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype CreateTypeTemplatesInputBuilder a = CreateTypeTemplatesInputBuilder {
    runCreateTypeTemplatesInputBuilder :: CreateTypeTemplatesInputBuilderState -> (CreateTypeTemplatesInputBuilderState, a)
}

instance Data.Functor.Functor CreateTypeTemplatesInputBuilder where
    fmap f (CreateTypeTemplatesInputBuilder g) =
        CreateTypeTemplatesInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CreateTypeTemplatesInputBuilder where
    pure a = CreateTypeTemplatesInputBuilder (\s -> (s, a))
    (CreateTypeTemplatesInputBuilder f) <*> (CreateTypeTemplatesInputBuilder g) = CreateTypeTemplatesInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CreateTypeTemplatesInputBuilder where
    (CreateTypeTemplatesInputBuilder f) >>= g = CreateTypeTemplatesInputBuilder (\s ->
        let (s', a) = f s
            (CreateTypeTemplatesInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> CreateTypeTemplatesInputBuilder ()
setWorkspaceId value =
   CreateTypeTemplatesInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> CreateTypeTemplatesInputBuilder ()
setOrgId value =
   CreateTypeTemplatesInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setTypeName :: Data.Text.Text -> CreateTypeTemplatesInputBuilder ()
setTypeName value =
   CreateTypeTemplatesInputBuilder (\s -> (s { type_nameBuilderState = Data.Maybe.Just value }, ()))

setTypeSchema :: Data.Aeson.Value -> CreateTypeTemplatesInputBuilder ()
setTypeSchema value =
   CreateTypeTemplatesInputBuilder (\s -> (s { type_schemaBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> CreateTypeTemplatesInputBuilder ()
setDescription value =
   CreateTypeTemplatesInputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> CreateTypeTemplatesInputBuilder ()
setChangeReason value =
   CreateTypeTemplatesInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: CreateTypeTemplatesInputBuilder () -> Data.Either.Either Data.Text.Text CreateTypeTemplatesInput
build builder = do
    let (st, _) = runCreateTypeTemplatesInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateTypeTemplatesInput.CreateTypeTemplatesInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateTypeTemplatesInput.CreateTypeTemplatesInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    type_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateTypeTemplatesInput.CreateTypeTemplatesInput.type_name is a required property.") Data.Either.Right (type_nameBuilderState st)
    type_schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateTypeTemplatesInput.CreateTypeTemplatesInput.type_schema is a required property.") Data.Either.Right (type_schemaBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateTypeTemplatesInput.CreateTypeTemplatesInput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateTypeTemplatesInput.CreateTypeTemplatesInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (CreateTypeTemplatesInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        type_name = type_name',
        type_schema = type_schema',
        description = description',
        change_reason = change_reason'
    })



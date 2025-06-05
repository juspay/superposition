module Io.Superposition.Model.UpdateTypeTemplatesInput (
    setWorkspaceId,
    setOrgId,
    setTypeName,
    setTypeSchema,
    setDescription,
    setChangeReason,
    build,
    UpdateTypeTemplatesInputBuilder,
    UpdateTypeTemplatesInput,
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

data UpdateTypeTemplatesInput = UpdateTypeTemplatesInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    type_name :: Data.Text.Text,
    type_schema :: Data.Aeson.Value,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateTypeTemplatesInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "type_name" Data.Aeson..= type_name a,
        "type_schema" Data.Aeson..= type_schema a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON UpdateTypeTemplatesInput where
    parseJSON = Data.Aeson.withObject "UpdateTypeTemplatesInput" $ \v -> UpdateTypeTemplatesInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "type_name")
        Control.Applicative.<*> (v Data.Aeson..: "type_schema")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data UpdateTypeTemplatesInputBuilderState = UpdateTypeTemplatesInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    type_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    type_schemaBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateTypeTemplatesInputBuilderState
defaultBuilderState = UpdateTypeTemplatesInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    type_nameBuilderState = Data.Maybe.Nothing,
    type_schemaBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype UpdateTypeTemplatesInputBuilder a = UpdateTypeTemplatesInputBuilder {
    runUpdateTypeTemplatesInputBuilder :: UpdateTypeTemplatesInputBuilderState -> (UpdateTypeTemplatesInputBuilderState, a)
}

instance Data.Functor.Functor UpdateTypeTemplatesInputBuilder where
    fmap f (UpdateTypeTemplatesInputBuilder g) =
        UpdateTypeTemplatesInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateTypeTemplatesInputBuilder where
    pure a = UpdateTypeTemplatesInputBuilder (\s -> (s, a))
    (UpdateTypeTemplatesInputBuilder f) <*> (UpdateTypeTemplatesInputBuilder g) = UpdateTypeTemplatesInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateTypeTemplatesInputBuilder where
    (UpdateTypeTemplatesInputBuilder f) >>= g = UpdateTypeTemplatesInputBuilder (\s ->
        let (s', a) = f s
            (UpdateTypeTemplatesInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> UpdateTypeTemplatesInputBuilder ()
setWorkspaceId value =
   UpdateTypeTemplatesInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> UpdateTypeTemplatesInputBuilder ()
setOrgId value =
   UpdateTypeTemplatesInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setTypeName :: Data.Text.Text -> UpdateTypeTemplatesInputBuilder ()
setTypeName value =
   UpdateTypeTemplatesInputBuilder (\s -> (s { type_nameBuilderState = Data.Maybe.Just value }, ()))

setTypeSchema :: Data.Aeson.Value -> UpdateTypeTemplatesInputBuilder ()
setTypeSchema value =
   UpdateTypeTemplatesInputBuilder (\s -> (s { type_schemaBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> UpdateTypeTemplatesInputBuilder ()
setDescription value =
   UpdateTypeTemplatesInputBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setChangeReason :: Data.Text.Text -> UpdateTypeTemplatesInputBuilder ()
setChangeReason value =
   UpdateTypeTemplatesInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: UpdateTypeTemplatesInputBuilder () -> Data.Either.Either Data.Text.Text UpdateTypeTemplatesInput
build builder = do
    let (st, _) = runUpdateTypeTemplatesInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateTypeTemplatesInput.UpdateTypeTemplatesInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateTypeTemplatesInput.UpdateTypeTemplatesInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    type_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateTypeTemplatesInput.UpdateTypeTemplatesInput.type_name is a required property.") Data.Either.Right (type_nameBuilderState st)
    type_schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateTypeTemplatesInput.UpdateTypeTemplatesInput.type_schema is a required property.") Data.Either.Right (type_schemaBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateTypeTemplatesInput.UpdateTypeTemplatesInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (UpdateTypeTemplatesInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        type_name = type_name',
        type_schema = type_schema',
        description = description',
        change_reason = change_reason'
    })



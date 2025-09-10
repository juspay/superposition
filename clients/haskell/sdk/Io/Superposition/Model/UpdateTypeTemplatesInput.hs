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
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

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
    

instance Io.Superposition.Utility.SerializeBody UpdateTypeTemplatesInput

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

type UpdateTypeTemplatesInputBuilder = Control.Monad.State.Strict.State UpdateTypeTemplatesInputBuilderState

setWorkspaceId :: Data.Text.Text -> UpdateTypeTemplatesInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> UpdateTypeTemplatesInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setTypeName :: Data.Text.Text -> UpdateTypeTemplatesInputBuilder ()
setTypeName value =
   Control.Monad.State.Strict.modify (\s -> (s { type_nameBuilderState = Data.Maybe.Just value }))

setTypeSchema :: Data.Aeson.Value -> UpdateTypeTemplatesInputBuilder ()
setTypeSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { type_schemaBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> UpdateTypeTemplatesInputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = value }))

setChangeReason :: Data.Text.Text -> UpdateTypeTemplatesInputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

build :: UpdateTypeTemplatesInputBuilder () -> Data.Either.Either Data.Text.Text UpdateTypeTemplatesInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
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


instance Io.Superposition.Utility.IntoRequestBuilder UpdateTypeTemplatesInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPut
        Io.Superposition.Utility.setPath [
            "types",
            Io.Superposition.Utility.serializeElement (type_name self)
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "change_reason" (change_reason self)
        Io.Superposition.Utility.serField "type_schema" (type_schema self)
        Io.Superposition.Utility.serField "description" (description self)


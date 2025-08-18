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
    

instance Io.Superposition.Utility.SerializeBody CreateTypeTemplatesInput

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

type CreateTypeTemplatesInputBuilder = Control.Monad.State.Strict.State CreateTypeTemplatesInputBuilderState

setWorkspaceId :: Data.Text.Text -> CreateTypeTemplatesInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> CreateTypeTemplatesInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setTypeName :: Data.Text.Text -> CreateTypeTemplatesInputBuilder ()
setTypeName value =
   Control.Monad.State.Strict.modify (\s -> (s { type_nameBuilderState = Data.Maybe.Just value }))

setTypeSchema :: Data.Aeson.Value -> CreateTypeTemplatesInputBuilder ()
setTypeSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { type_schemaBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> CreateTypeTemplatesInputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> CreateTypeTemplatesInputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

build :: CreateTypeTemplatesInputBuilder () -> Data.Either.Either Data.Text.Text CreateTypeTemplatesInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
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


instance Io.Superposition.Utility.IntoRequestBuilder CreateTypeTemplatesInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "types"
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "type_name" (type_name self)
        Io.Superposition.Utility.serField "change_reason" (change_reason self)
        Io.Superposition.Utility.serField "type_schema" (type_schema self)
        Io.Superposition.Utility.serField "description" (description self)


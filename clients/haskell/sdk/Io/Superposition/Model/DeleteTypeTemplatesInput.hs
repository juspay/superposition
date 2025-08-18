module Io.Superposition.Model.DeleteTypeTemplatesInput (
    setWorkspaceId,
    setOrgId,
    setTypeName,
    build,
    DeleteTypeTemplatesInputBuilder,
    DeleteTypeTemplatesInput,
    workspace_id,
    org_id,
    type_name
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

data DeleteTypeTemplatesInput = DeleteTypeTemplatesInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    type_name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteTypeTemplatesInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "type_name" Data.Aeson..= type_name a
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteTypeTemplatesInput

instance Data.Aeson.FromJSON DeleteTypeTemplatesInput where
    parseJSON = Data.Aeson.withObject "DeleteTypeTemplatesInput" $ \v -> DeleteTypeTemplatesInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "type_name")
    



data DeleteTypeTemplatesInputBuilderState = DeleteTypeTemplatesInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    type_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteTypeTemplatesInputBuilderState
defaultBuilderState = DeleteTypeTemplatesInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    type_nameBuilderState = Data.Maybe.Nothing
}

type DeleteTypeTemplatesInputBuilder = Control.Monad.State.Strict.State DeleteTypeTemplatesInputBuilderState

setWorkspaceId :: Data.Text.Text -> DeleteTypeTemplatesInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> DeleteTypeTemplatesInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setTypeName :: Data.Text.Text -> DeleteTypeTemplatesInputBuilder ()
setTypeName value =
   Control.Monad.State.Strict.modify (\s -> (s { type_nameBuilderState = Data.Maybe.Just value }))

build :: DeleteTypeTemplatesInputBuilder () -> Data.Either.Either Data.Text.Text DeleteTypeTemplatesInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesInput.DeleteTypeTemplatesInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesInput.DeleteTypeTemplatesInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    type_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesInput.DeleteTypeTemplatesInput.type_name is a required property.") Data.Either.Right (type_nameBuilderState st)
    Data.Either.Right (DeleteTypeTemplatesInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        type_name = type_name'
    })


instance Io.Superposition.Utility.IntoRequestBuilder DeleteTypeTemplatesInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodDelete
        Io.Superposition.Utility.setPath [
            "types",
            Io.Superposition.Utility.serializeElement (type_name self)
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


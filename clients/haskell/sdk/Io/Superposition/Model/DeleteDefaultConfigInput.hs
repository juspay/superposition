module Io.Superposition.Model.DeleteDefaultConfigInput (
    setWorkspaceId,
    setOrgId,
    setKey,
    build,
    DeleteDefaultConfigInputBuilder,
    DeleteDefaultConfigInput,
    workspace_id,
    org_id,
    key
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

data DeleteDefaultConfigInput = DeleteDefaultConfigInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    key :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteDefaultConfigInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "key" Data.Aeson..= key a
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteDefaultConfigInput

instance Data.Aeson.FromJSON DeleteDefaultConfigInput where
    parseJSON = Data.Aeson.withObject "DeleteDefaultConfigInput" $ \v -> DeleteDefaultConfigInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "key")
    



data DeleteDefaultConfigInputBuilderState = DeleteDefaultConfigInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteDefaultConfigInputBuilderState
defaultBuilderState = DeleteDefaultConfigInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    keyBuilderState = Data.Maybe.Nothing
}

type DeleteDefaultConfigInputBuilder = Control.Monad.State.Strict.State DeleteDefaultConfigInputBuilderState

setWorkspaceId :: Data.Text.Text -> DeleteDefaultConfigInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> DeleteDefaultConfigInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setKey :: Data.Text.Text -> DeleteDefaultConfigInputBuilder ()
setKey value =
   Control.Monad.State.Strict.modify (\s -> (s { keyBuilderState = Data.Maybe.Just value }))

build :: DeleteDefaultConfigInputBuilder () -> Data.Either.Either Data.Text.Text DeleteDefaultConfigInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteDefaultConfigInput.DeleteDefaultConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteDefaultConfigInput.DeleteDefaultConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteDefaultConfigInput.DeleteDefaultConfigInput.key is a required property.") Data.Either.Right (keyBuilderState st)
    Data.Either.Right (DeleteDefaultConfigInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        key = key'
    })


instance Io.Superposition.Utility.IntoRequestBuilder DeleteDefaultConfigInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodDelete
        Io.Superposition.Utility.setPath [
            "default-config",
            Io.Superposition.Utility.serializeElement (key self)
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


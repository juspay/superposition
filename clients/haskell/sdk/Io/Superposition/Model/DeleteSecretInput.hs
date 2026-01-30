module Io.Superposition.Model.DeleteSecretInput (
    setWorkspaceId,
    setOrgId,
    setName,
    build,
    DeleteSecretInputBuilder,
    DeleteSecretInput,
    workspace_id,
    org_id,
    name
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

data DeleteSecretInput = DeleteSecretInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteSecretInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "name" Data.Aeson..= name a
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteSecretInput

instance Data.Aeson.FromJSON DeleteSecretInput where
    parseJSON = Data.Aeson.withObject "DeleteSecretInput" $ \v -> DeleteSecretInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "name")
    



data DeleteSecretInputBuilderState = DeleteSecretInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteSecretInputBuilderState
defaultBuilderState = DeleteSecretInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing
}

type DeleteSecretInputBuilder = Control.Monad.State.Strict.State DeleteSecretInputBuilderState

setWorkspaceId :: Data.Text.Text -> DeleteSecretInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> DeleteSecretInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> DeleteSecretInputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

build :: DeleteSecretInputBuilder () -> Data.Either.Either Data.Text.Text DeleteSecretInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteSecretInput.DeleteSecretInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteSecretInput.DeleteSecretInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteSecretInput.DeleteSecretInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    Data.Either.Right (DeleteSecretInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        name = name'
    })


instance Io.Superposition.Utility.IntoRequestBuilder DeleteSecretInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodDelete
        Io.Superposition.Utility.setPath [
            "secrets",
            Io.Superposition.Utility.serializeElement (name self)
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


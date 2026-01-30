module Io.Superposition.Model.RotateWorkspaceEncryptionKeyInput (
    setOrgId,
    setWorkspaceName,
    build,
    RotateWorkspaceEncryptionKeyInputBuilder,
    RotateWorkspaceEncryptionKeyInput,
    org_id,
    workspace_name
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

data RotateWorkspaceEncryptionKeyInput = RotateWorkspaceEncryptionKeyInput {
    org_id :: Data.Text.Text,
    workspace_name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON RotateWorkspaceEncryptionKeyInput where
    toJSON a = Data.Aeson.object [
        "org_id" Data.Aeson..= org_id a,
        "workspace_name" Data.Aeson..= workspace_name a
        ]
    

instance Io.Superposition.Utility.SerializeBody RotateWorkspaceEncryptionKeyInput

instance Data.Aeson.FromJSON RotateWorkspaceEncryptionKeyInput where
    parseJSON = Data.Aeson.withObject "RotateWorkspaceEncryptionKeyInput" $ \v -> RotateWorkspaceEncryptionKeyInput
        Data.Functor.<$> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_name")
    



data RotateWorkspaceEncryptionKeyInputBuilderState = RotateWorkspaceEncryptionKeyInputBuilderState {
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: RotateWorkspaceEncryptionKeyInputBuilderState
defaultBuilderState = RotateWorkspaceEncryptionKeyInputBuilderState {
    org_idBuilderState = Data.Maybe.Nothing,
    workspace_nameBuilderState = Data.Maybe.Nothing
}

type RotateWorkspaceEncryptionKeyInputBuilder = Control.Monad.State.Strict.State RotateWorkspaceEncryptionKeyInputBuilderState

setOrgId :: Data.Text.Text -> RotateWorkspaceEncryptionKeyInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setWorkspaceName :: Data.Text.Text -> RotateWorkspaceEncryptionKeyInputBuilder ()
setWorkspaceName value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_nameBuilderState = Data.Maybe.Just value }))

build :: RotateWorkspaceEncryptionKeyInputBuilder () -> Data.Either.Either Data.Text.Text RotateWorkspaceEncryptionKeyInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RotateWorkspaceEncryptionKeyInput.RotateWorkspaceEncryptionKeyInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    workspace_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RotateWorkspaceEncryptionKeyInput.RotateWorkspaceEncryptionKeyInput.workspace_name is a required property.") Data.Either.Right (workspace_nameBuilderState st)
    Data.Either.Right (RotateWorkspaceEncryptionKeyInput { 
        org_id = org_id',
        workspace_name = workspace_name'
    })


instance Io.Superposition.Utility.IntoRequestBuilder RotateWorkspaceEncryptionKeyInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "workspaces",
            Io.Superposition.Utility.serializeElement (workspace_name self),
            "rotate-encryption-key"
            ]
        
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


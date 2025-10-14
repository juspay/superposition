module Io.Superposition.Model.GetWorkspaceInput (
    setOrgId,
    setWorkspaceName,
    build,
    GetWorkspaceInputBuilder,
    GetWorkspaceInput,
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

data GetWorkspaceInput = GetWorkspaceInput {
    org_id :: Data.Text.Text,
    workspace_name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetWorkspaceInput where
    toJSON a = Data.Aeson.object [
        "org_id" Data.Aeson..= org_id a,
        "workspace_name" Data.Aeson..= workspace_name a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetWorkspaceInput

instance Data.Aeson.FromJSON GetWorkspaceInput where
    parseJSON = Data.Aeson.withObject "GetWorkspaceInput" $ \v -> GetWorkspaceInput
        Data.Functor.<$> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_name")
    



data GetWorkspaceInputBuilderState = GetWorkspaceInputBuilderState {
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetWorkspaceInputBuilderState
defaultBuilderState = GetWorkspaceInputBuilderState {
    org_idBuilderState = Data.Maybe.Nothing,
    workspace_nameBuilderState = Data.Maybe.Nothing
}

type GetWorkspaceInputBuilder = Control.Monad.State.Strict.State GetWorkspaceInputBuilderState

setOrgId :: Data.Text.Text -> GetWorkspaceInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setWorkspaceName :: Data.Text.Text -> GetWorkspaceInputBuilder ()
setWorkspaceName value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_nameBuilderState = Data.Maybe.Just value }))

build :: GetWorkspaceInputBuilder () -> Data.Either.Either Data.Text.Text GetWorkspaceInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceInput.GetWorkspaceInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    workspace_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWorkspaceInput.GetWorkspaceInput.workspace_name is a required property.") Data.Either.Right (workspace_nameBuilderState st)
    Data.Either.Right (GetWorkspaceInput { 
        org_id = org_id',
        workspace_name = workspace_name'
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetWorkspaceInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "workspaces",
            Io.Superposition.Utility.serializeElement (workspace_name self)
            ]
        
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


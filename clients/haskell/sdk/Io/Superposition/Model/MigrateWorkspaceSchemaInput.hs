module Io.Superposition.Model.MigrateWorkspaceSchemaInput (
    setOrgId,
    setWorkspaceName,
    build,
    MigrateWorkspaceSchemaInputBuilder,
    MigrateWorkspaceSchemaInput,
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

data MigrateWorkspaceSchemaInput = MigrateWorkspaceSchemaInput {
    org_id :: Data.Text.Text,
    workspace_name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON MigrateWorkspaceSchemaInput where
    toJSON a = Data.Aeson.object [
        "org_id" Data.Aeson..= org_id a,
        "workspace_name" Data.Aeson..= workspace_name a
        ]
    

instance Io.Superposition.Utility.SerializeBody MigrateWorkspaceSchemaInput

instance Data.Aeson.FromJSON MigrateWorkspaceSchemaInput where
    parseJSON = Data.Aeson.withObject "MigrateWorkspaceSchemaInput" $ \v -> MigrateWorkspaceSchemaInput
        Data.Functor.<$> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_name")
    



data MigrateWorkspaceSchemaInputBuilderState = MigrateWorkspaceSchemaInputBuilderState {
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: MigrateWorkspaceSchemaInputBuilderState
defaultBuilderState = MigrateWorkspaceSchemaInputBuilderState {
    org_idBuilderState = Data.Maybe.Nothing,
    workspace_nameBuilderState = Data.Maybe.Nothing
}

type MigrateWorkspaceSchemaInputBuilder = Control.Monad.State.Strict.State MigrateWorkspaceSchemaInputBuilderState

setOrgId :: Data.Text.Text -> MigrateWorkspaceSchemaInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setWorkspaceName :: Data.Text.Text -> MigrateWorkspaceSchemaInputBuilder ()
setWorkspaceName value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_nameBuilderState = Data.Maybe.Just value }))

build :: MigrateWorkspaceSchemaInputBuilder () -> Data.Either.Either Data.Text.Text MigrateWorkspaceSchemaInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MigrateWorkspaceSchemaInput.MigrateWorkspaceSchemaInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    workspace_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MigrateWorkspaceSchemaInput.MigrateWorkspaceSchemaInput.workspace_name is a required property.") Data.Either.Right (workspace_nameBuilderState st)
    Data.Either.Right (MigrateWorkspaceSchemaInput { 
        org_id = org_id',
        workspace_name = workspace_name'
    })


instance Io.Superposition.Utility.IntoRequestBuilder MigrateWorkspaceSchemaInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "workspaces",
            Io.Superposition.Utility.serializeElement (workspace_name self),
            "db",
            "migrate"
            ]
        
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


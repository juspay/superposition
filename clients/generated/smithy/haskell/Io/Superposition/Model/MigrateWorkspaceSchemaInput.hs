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
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

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

newtype MigrateWorkspaceSchemaInputBuilder a = MigrateWorkspaceSchemaInputBuilder {
    runMigrateWorkspaceSchemaInputBuilder :: MigrateWorkspaceSchemaInputBuilderState -> (MigrateWorkspaceSchemaInputBuilderState, a)
}

instance Data.Functor.Functor MigrateWorkspaceSchemaInputBuilder where
    fmap f (MigrateWorkspaceSchemaInputBuilder g) =
        MigrateWorkspaceSchemaInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative MigrateWorkspaceSchemaInputBuilder where
    pure a = MigrateWorkspaceSchemaInputBuilder (\s -> (s, a))
    (MigrateWorkspaceSchemaInputBuilder f) <*> (MigrateWorkspaceSchemaInputBuilder g) = MigrateWorkspaceSchemaInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad MigrateWorkspaceSchemaInputBuilder where
    (MigrateWorkspaceSchemaInputBuilder f) >>= g = MigrateWorkspaceSchemaInputBuilder (\s ->
        let (s', a) = f s
            (MigrateWorkspaceSchemaInputBuilder h) = g a
        in h s')

setOrgId :: Data.Text.Text -> MigrateWorkspaceSchemaInputBuilder ()
setOrgId value =
   MigrateWorkspaceSchemaInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setWorkspaceName :: Data.Text.Text -> MigrateWorkspaceSchemaInputBuilder ()
setWorkspaceName value =
   MigrateWorkspaceSchemaInputBuilder (\s -> (s { workspace_nameBuilderState = Data.Maybe.Just value }, ()))

build :: MigrateWorkspaceSchemaInputBuilder () -> Data.Either.Either Data.Text.Text MigrateWorkspaceSchemaInput
build builder = do
    let (st, _) = runMigrateWorkspaceSchemaInputBuilder builder defaultBuilderState
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MigrateWorkspaceSchemaInput.MigrateWorkspaceSchemaInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    workspace_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MigrateWorkspaceSchemaInput.MigrateWorkspaceSchemaInput.workspace_name is a required property.") Data.Either.Right (workspace_nameBuilderState st)
    Data.Either.Right (MigrateWorkspaceSchemaInput { 
        org_id = org_id',
        workspace_name = workspace_name'
    })



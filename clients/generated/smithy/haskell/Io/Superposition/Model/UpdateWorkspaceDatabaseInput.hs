module Io.Superposition.Model.UpdateWorkspaceDatabaseInput (
    setOrgId,
    setWorkspaceName,
    build,
    UpdateWorkspaceDatabaseInputBuilder,
    UpdateWorkspaceDatabaseInput,
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

data UpdateWorkspaceDatabaseInput = UpdateWorkspaceDatabaseInput {
    org_id :: Data.Text.Text,
    workspace_name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateWorkspaceDatabaseInput where
    toJSON a = Data.Aeson.object [
        "org_id" Data.Aeson..= org_id a,
        "workspace_name" Data.Aeson..= workspace_name a
        ]
    


instance Data.Aeson.FromJSON UpdateWorkspaceDatabaseInput where
    parseJSON = Data.Aeson.withObject "UpdateWorkspaceDatabaseInput" $ \v -> UpdateWorkspaceDatabaseInput
        Data.Functor.<$> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_name")
    



data UpdateWorkspaceDatabaseInputBuilderState = UpdateWorkspaceDatabaseInputBuilderState {
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    workspace_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateWorkspaceDatabaseInputBuilderState
defaultBuilderState = UpdateWorkspaceDatabaseInputBuilderState {
    org_idBuilderState = Data.Maybe.Nothing,
    workspace_nameBuilderState = Data.Maybe.Nothing
}

newtype UpdateWorkspaceDatabaseInputBuilder a = UpdateWorkspaceDatabaseInputBuilder {
    runUpdateWorkspaceDatabaseInputBuilder :: UpdateWorkspaceDatabaseInputBuilderState -> (UpdateWorkspaceDatabaseInputBuilderState, a)
}

instance Data.Functor.Functor UpdateWorkspaceDatabaseInputBuilder where
    fmap f (UpdateWorkspaceDatabaseInputBuilder g) =
        UpdateWorkspaceDatabaseInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateWorkspaceDatabaseInputBuilder where
    pure a = UpdateWorkspaceDatabaseInputBuilder (\s -> (s, a))
    (UpdateWorkspaceDatabaseInputBuilder f) <*> (UpdateWorkspaceDatabaseInputBuilder g) = UpdateWorkspaceDatabaseInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateWorkspaceDatabaseInputBuilder where
    (UpdateWorkspaceDatabaseInputBuilder f) >>= g = UpdateWorkspaceDatabaseInputBuilder (\s ->
        let (s', a) = f s
            (UpdateWorkspaceDatabaseInputBuilder h) = g a
        in h s')

setOrgId :: Data.Text.Text -> UpdateWorkspaceDatabaseInputBuilder ()
setOrgId value =
   UpdateWorkspaceDatabaseInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setWorkspaceName :: Data.Text.Text -> UpdateWorkspaceDatabaseInputBuilder ()
setWorkspaceName value =
   UpdateWorkspaceDatabaseInputBuilder (\s -> (s { workspace_nameBuilderState = Data.Maybe.Just value }, ()))

build :: UpdateWorkspaceDatabaseInputBuilder () -> Data.Either.Either Data.Text.Text UpdateWorkspaceDatabaseInput
build builder = do
    let (st, _) = runUpdateWorkspaceDatabaseInputBuilder builder defaultBuilderState
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseInput.UpdateWorkspaceDatabaseInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    workspace_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWorkspaceDatabaseInput.UpdateWorkspaceDatabaseInput.workspace_name is a required property.") Data.Either.Right (workspace_nameBuilderState st)
    Data.Either.Right (UpdateWorkspaceDatabaseInput { 
        org_id = org_id',
        workspace_name = workspace_name'
    })



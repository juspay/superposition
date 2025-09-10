module Io.Superposition.Model.DeleteExperimentGroupInput (
    setWorkspaceId,
    setOrgId,
    setId',
    build,
    DeleteExperimentGroupInputBuilder,
    DeleteExperimentGroupInput,
    workspace_id,
    org_id,
    id'
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

data DeleteExperimentGroupInput = DeleteExperimentGroupInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteExperimentGroupInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteExperimentGroupInput

instance Data.Aeson.FromJSON DeleteExperimentGroupInput where
    parseJSON = Data.Aeson.withObject "DeleteExperimentGroupInput" $ \v -> DeleteExperimentGroupInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
    



data DeleteExperimentGroupInputBuilderState = DeleteExperimentGroupInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteExperimentGroupInputBuilderState
defaultBuilderState = DeleteExperimentGroupInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing
}

type DeleteExperimentGroupInputBuilder = Control.Monad.State.Strict.State DeleteExperimentGroupInputBuilderState

setWorkspaceId :: Data.Text.Text -> DeleteExperimentGroupInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> DeleteExperimentGroupInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setId' :: Data.Text.Text -> DeleteExperimentGroupInputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

build :: DeleteExperimentGroupInputBuilder () -> Data.Either.Either Data.Text.Text DeleteExperimentGroupInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupInput.DeleteExperimentGroupInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupInput.DeleteExperimentGroupInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupInput.DeleteExperimentGroupInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    Data.Either.Right (DeleteExperimentGroupInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id''
    })


instance Io.Superposition.Utility.IntoRequestBuilder DeleteExperimentGroupInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodDelete
        Io.Superposition.Utility.setPath [
            "experiment-groups",
            Io.Superposition.Utility.serializeElement (id' self)
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


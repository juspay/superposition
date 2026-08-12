module Io.Superposition.Model.CancelJobInput (
    setWorkspaceId,
    setOrgId,
    setId',
    build,
    CancelJobInputBuilder,
    CancelJobInput,
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

data CancelJobInput = CancelJobInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CancelJobInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a
        ]
    

instance Io.Superposition.Utility.SerializeBody CancelJobInput

instance Data.Aeson.FromJSON CancelJobInput where
    parseJSON = Data.Aeson.withObject "CancelJobInput" $ \v -> CancelJobInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
    



data CancelJobInputBuilderState = CancelJobInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CancelJobInputBuilderState
defaultBuilderState = CancelJobInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing
}

type CancelJobInputBuilder = Control.Monad.State.Strict.State CancelJobInputBuilderState

setWorkspaceId :: Data.Text.Text -> CancelJobInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> CancelJobInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setId' :: Data.Text.Text -> CancelJobInputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

build :: CancelJobInputBuilder () -> Data.Either.Either Data.Text.Text CancelJobInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CancelJobInput.CancelJobInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CancelJobInput.CancelJobInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CancelJobInput.CancelJobInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    Data.Either.Right (CancelJobInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id''
    })


instance Io.Superposition.Utility.IntoRequestBuilder CancelJobInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "jobs",
            Io.Superposition.Utility.serializeElement (id' self),
            "cancel"
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


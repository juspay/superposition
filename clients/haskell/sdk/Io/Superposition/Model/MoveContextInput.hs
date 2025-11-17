module Io.Superposition.Model.MoveContextInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setRequest,
    build,
    MoveContextInputBuilder,
    MoveContextInput,
    workspace_id,
    org_id,
    id',
    request
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
import qualified Io.Superposition.Model.ContextMove
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data MoveContextInput = MoveContextInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    request :: Io.Superposition.Model.ContextMove.ContextMove
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON MoveContextInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "request" Data.Aeson..= request a
        ]
    

instance Io.Superposition.Utility.SerializeBody MoveContextInput

instance Data.Aeson.FromJSON MoveContextInput where
    parseJSON = Data.Aeson.withObject "MoveContextInput" $ \v -> MoveContextInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "request")
    



data MoveContextInputBuilderState = MoveContextInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    requestBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ContextMove.ContextMove
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: MoveContextInputBuilderState
defaultBuilderState = MoveContextInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    requestBuilderState = Data.Maybe.Nothing
}

type MoveContextInputBuilder = Control.Monad.State.Strict.State MoveContextInputBuilderState

setWorkspaceId :: Data.Text.Text -> MoveContextInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> MoveContextInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setId' :: Data.Text.Text -> MoveContextInputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setRequest :: Io.Superposition.Model.ContextMove.ContextMove -> MoveContextInputBuilder ()
setRequest value =
   Control.Monad.State.Strict.modify (\s -> (s { requestBuilderState = Data.Maybe.Just value }))

build :: MoveContextInputBuilder () -> Data.Either.Either Data.Text.Text MoveContextInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextInput.MoveContextInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextInput.MoveContextInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextInput.MoveContextInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    request' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextInput.MoveContextInput.request is a required property.") Data.Either.Right (requestBuilderState st)
    Data.Either.Right (MoveContextInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        request = request'
    })


instance Io.Superposition.Utility.IntoRequestBuilder MoveContextInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPut
        Io.Superposition.Utility.setPath [
            "context",
            "move",
            Io.Superposition.Utility.serializeElement (id' self)
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serBody "application/json" (request self)


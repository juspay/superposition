module Io.Superposition.Model.GetConfigFastInput (
    setWorkspaceId,
    setOrgId,
    build,
    GetConfigFastInputBuilder,
    GetConfigFastInput,
    workspace_id,
    org_id
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

data GetConfigFastInput = GetConfigFastInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetConfigFastInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetConfigFastInput

instance Data.Aeson.FromJSON GetConfigFastInput where
    parseJSON = Data.Aeson.withObject "GetConfigFastInput" $ \v -> GetConfigFastInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
    



data GetConfigFastInputBuilderState = GetConfigFastInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetConfigFastInputBuilderState
defaultBuilderState = GetConfigFastInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing
}

type GetConfigFastInputBuilder = Control.Monad.State.Strict.State GetConfigFastInputBuilderState

setWorkspaceId :: Data.Text.Text -> GetConfigFastInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> GetConfigFastInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

build :: GetConfigFastInputBuilder () -> Data.Either.Either Data.Text.Text GetConfigFastInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetConfigFastInput.GetConfigFastInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetConfigFastInput.GetConfigFastInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    Data.Either.Right (GetConfigFastInput { 
        workspace_id = workspace_id',
        org_id = org_id'
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetConfigFastInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "config",
            "fast"
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


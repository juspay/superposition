module Io.Superposition.Model.GetWebhookInput (
    setWorkspaceId,
    setOrgId,
    setName,
    build,
    GetWebhookInputBuilder,
    GetWebhookInput,
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

data GetWebhookInput = GetWebhookInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetWebhookInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "name" Data.Aeson..= name a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetWebhookInput

instance Data.Aeson.FromJSON GetWebhookInput where
    parseJSON = Data.Aeson.withObject "GetWebhookInput" $ \v -> GetWebhookInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "name")
    



data GetWebhookInputBuilderState = GetWebhookInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetWebhookInputBuilderState
defaultBuilderState = GetWebhookInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing
}

type GetWebhookInputBuilder = Control.Monad.State.Strict.State GetWebhookInputBuilderState

setWorkspaceId :: Data.Text.Text -> GetWebhookInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> GetWebhookInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> GetWebhookInputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

build :: GetWebhookInputBuilder () -> Data.Either.Either Data.Text.Text GetWebhookInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookInput.GetWebhookInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookInput.GetWebhookInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookInput.GetWebhookInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    Data.Either.Right (GetWebhookInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        name = name'
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetWebhookInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "webhook",
            Io.Superposition.Utility.serializeElement (name self)
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


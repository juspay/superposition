module Io.Superposition.Model.GetWebhookByEventInput (
    setWorkspaceId,
    setOrgId,
    setEvent,
    build,
    GetWebhookByEventInputBuilder,
    GetWebhookByEventInput,
    workspace_id,
    org_id,
    event
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

data GetWebhookByEventInput = GetWebhookByEventInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    event :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetWebhookByEventInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "event" Data.Aeson..= event a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetWebhookByEventInput

instance Data.Aeson.FromJSON GetWebhookByEventInput where
    parseJSON = Data.Aeson.withObject "GetWebhookByEventInput" $ \v -> GetWebhookByEventInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "event")
    



data GetWebhookByEventInputBuilderState = GetWebhookByEventInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    eventBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetWebhookByEventInputBuilderState
defaultBuilderState = GetWebhookByEventInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    eventBuilderState = Data.Maybe.Nothing
}

type GetWebhookByEventInputBuilder = Control.Monad.State.Strict.State GetWebhookByEventInputBuilderState

setWorkspaceId :: Data.Text.Text -> GetWebhookByEventInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> GetWebhookByEventInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setEvent :: Data.Text.Text -> GetWebhookByEventInputBuilder ()
setEvent value =
   Control.Monad.State.Strict.modify (\s -> (s { eventBuilderState = Data.Maybe.Just value }))

build :: GetWebhookByEventInputBuilder () -> Data.Either.Either Data.Text.Text GetWebhookByEventInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookByEventInput.GetWebhookByEventInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookByEventInput.GetWebhookByEventInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    event' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookByEventInput.GetWebhookByEventInput.event is a required property.") Data.Either.Right (eventBuilderState st)
    Data.Either.Right (GetWebhookByEventInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        event = event'
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetWebhookByEventInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "webhook",
            "event",
            Io.Superposition.Utility.serializeElement (event self)
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


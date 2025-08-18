module Io.Superposition.Model.CreateWebhookInput (
    setWorkspaceId,
    setOrgId,
    setName,
    setDescription,
    setEnabled,
    setUrl,
    setMethod,
    setVersion,
    setCustomHeaders,
    setEvents,
    setChangeReason,
    build,
    CreateWebhookInputBuilder,
    CreateWebhookInput,
    workspace_id,
    org_id,
    name,
    description,
    enabled,
    url,
    method,
    version,
    custom_headers,
    events,
    change_reason
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.HttpMethod
import qualified Io.Superposition.Model.Version
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data CreateWebhookInput = CreateWebhookInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    name :: Data.Text.Text,
    description :: Data.Text.Text,
    enabled :: Bool,
    url :: Data.Text.Text,
    method :: Io.Superposition.Model.HttpMethod.HttpMethod,
    version :: Data.Maybe.Maybe Io.Superposition.Model.Version.Version,
    custom_headers :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    events :: [] Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateWebhookInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "name" Data.Aeson..= name a,
        "description" Data.Aeson..= description a,
        "enabled" Data.Aeson..= enabled a,
        "url" Data.Aeson..= url a,
        "method" Data.Aeson..= method a,
        "version" Data.Aeson..= version a,
        "custom_headers" Data.Aeson..= custom_headers a,
        "events" Data.Aeson..= events a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    

instance Io.Superposition.Utility.SerializeBody CreateWebhookInput

instance Data.Aeson.FromJSON CreateWebhookInput where
    parseJSON = Data.Aeson.withObject "CreateWebhookInput" $ \v -> CreateWebhookInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "enabled")
        Control.Applicative.<*> (v Data.Aeson..: "url")
        Control.Applicative.<*> (v Data.Aeson..: "method")
        Control.Applicative.<*> (v Data.Aeson..: "version")
        Control.Applicative.<*> (v Data.Aeson..: "custom_headers")
        Control.Applicative.<*> (v Data.Aeson..: "events")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data CreateWebhookInputBuilderState = CreateWebhookInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    enabledBuilderState :: Data.Maybe.Maybe Bool,
    urlBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    methodBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.HttpMethod.HttpMethod,
    versionBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.Version.Version,
    custom_headersBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    eventsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateWebhookInputBuilderState
defaultBuilderState = CreateWebhookInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    enabledBuilderState = Data.Maybe.Nothing,
    urlBuilderState = Data.Maybe.Nothing,
    methodBuilderState = Data.Maybe.Nothing,
    versionBuilderState = Data.Maybe.Nothing,
    custom_headersBuilderState = Data.Maybe.Nothing,
    eventsBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

type CreateWebhookInputBuilder = Control.Monad.State.Strict.State CreateWebhookInputBuilderState

setWorkspaceId :: Data.Text.Text -> CreateWebhookInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> CreateWebhookInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> CreateWebhookInputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> CreateWebhookInputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setEnabled :: Bool -> CreateWebhookInputBuilder ()
setEnabled value =
   Control.Monad.State.Strict.modify (\s -> (s { enabledBuilderState = Data.Maybe.Just value }))

setUrl :: Data.Text.Text -> CreateWebhookInputBuilder ()
setUrl value =
   Control.Monad.State.Strict.modify (\s -> (s { urlBuilderState = Data.Maybe.Just value }))

setMethod :: Io.Superposition.Model.HttpMethod.HttpMethod -> CreateWebhookInputBuilder ()
setMethod value =
   Control.Monad.State.Strict.modify (\s -> (s { methodBuilderState = Data.Maybe.Just value }))

setVersion :: Data.Maybe.Maybe Io.Superposition.Model.Version.Version -> CreateWebhookInputBuilder ()
setVersion value =
   Control.Monad.State.Strict.modify (\s -> (s { versionBuilderState = value }))

setCustomHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> CreateWebhookInputBuilder ()
setCustomHeaders value =
   Control.Monad.State.Strict.modify (\s -> (s { custom_headersBuilderState = value }))

setEvents :: [] Data.Text.Text -> CreateWebhookInputBuilder ()
setEvents value =
   Control.Monad.State.Strict.modify (\s -> (s { eventsBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> CreateWebhookInputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

build :: CreateWebhookInputBuilder () -> Data.Either.Either Data.Text.Text CreateWebhookInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    enabled' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput.enabled is a required property.") Data.Either.Right (enabledBuilderState st)
    url' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput.url is a required property.") Data.Either.Right (urlBuilderState st)
    method' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput.method is a required property.") Data.Either.Right (methodBuilderState st)
    version' <- Data.Either.Right (versionBuilderState st)
    custom_headers' <- Data.Either.Right (custom_headersBuilderState st)
    events' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput.events is a required property.") Data.Either.Right (eventsBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (CreateWebhookInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        name = name',
        description = description',
        enabled = enabled',
        url = url',
        method = method',
        version = version',
        custom_headers = custom_headers',
        events = events',
        change_reason = change_reason'
    })


instance Io.Superposition.Utility.IntoRequestBuilder CreateWebhookInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "webhook"
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "change_reason" (change_reason self)
        Io.Superposition.Utility.serField "method" (method self)
        Io.Superposition.Utility.serField "name" (name self)
        Io.Superposition.Utility.serField "description" (description self)
        Io.Superposition.Utility.serField "version" (version self)
        Io.Superposition.Utility.serField "enabled" (enabled self)
        Io.Superposition.Utility.serField "url" (url self)
        Io.Superposition.Utility.serField "events" (events self)
        Io.Superposition.Utility.serField "custom_headers" (custom_headers self)


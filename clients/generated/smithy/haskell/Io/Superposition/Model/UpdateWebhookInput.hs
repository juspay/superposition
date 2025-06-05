module Io.Superposition.Model.UpdateWebhookInput (
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
    UpdateWebhookInputBuilder,
    UpdateWebhookInput,
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
import qualified Control.Monad
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

data UpdateWebhookInput = UpdateWebhookInput {
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

instance Data.Aeson.ToJSON UpdateWebhookInput where
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
    


instance Data.Aeson.FromJSON UpdateWebhookInput where
    parseJSON = Data.Aeson.withObject "UpdateWebhookInput" $ \v -> UpdateWebhookInput
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
    



data UpdateWebhookInputBuilderState = UpdateWebhookInputBuilderState {
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

defaultBuilderState :: UpdateWebhookInputBuilderState
defaultBuilderState = UpdateWebhookInputBuilderState {
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

newtype UpdateWebhookInputBuilder a = UpdateWebhookInputBuilder {
    runUpdateWebhookInputBuilder :: UpdateWebhookInputBuilderState -> (UpdateWebhookInputBuilderState, a)
}

instance Data.Functor.Functor UpdateWebhookInputBuilder where
    fmap f (UpdateWebhookInputBuilder g) =
        UpdateWebhookInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateWebhookInputBuilder where
    pure a = UpdateWebhookInputBuilder (\s -> (s, a))
    (UpdateWebhookInputBuilder f) <*> (UpdateWebhookInputBuilder g) = UpdateWebhookInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateWebhookInputBuilder where
    (UpdateWebhookInputBuilder f) >>= g = UpdateWebhookInputBuilder (\s ->
        let (s', a) = f s
            (UpdateWebhookInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> UpdateWebhookInputBuilder ()
setWorkspaceId value =
   UpdateWebhookInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> UpdateWebhookInputBuilder ()
setOrgId value =
   UpdateWebhookInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setName :: Data.Text.Text -> UpdateWebhookInputBuilder ()
setName value =
   UpdateWebhookInputBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> UpdateWebhookInputBuilder ()
setDescription value =
   UpdateWebhookInputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setEnabled :: Bool -> UpdateWebhookInputBuilder ()
setEnabled value =
   UpdateWebhookInputBuilder (\s -> (s { enabledBuilderState = Data.Maybe.Just value }, ()))

setUrl :: Data.Text.Text -> UpdateWebhookInputBuilder ()
setUrl value =
   UpdateWebhookInputBuilder (\s -> (s { urlBuilderState = Data.Maybe.Just value }, ()))

setMethod :: Io.Superposition.Model.HttpMethod.HttpMethod -> UpdateWebhookInputBuilder ()
setMethod value =
   UpdateWebhookInputBuilder (\s -> (s { methodBuilderState = Data.Maybe.Just value }, ()))

setVersion :: Data.Maybe.Maybe Io.Superposition.Model.Version.Version -> UpdateWebhookInputBuilder ()
setVersion value =
   UpdateWebhookInputBuilder (\s -> (s { versionBuilderState = value }, ()))

setCustomHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> UpdateWebhookInputBuilder ()
setCustomHeaders value =
   UpdateWebhookInputBuilder (\s -> (s { custom_headersBuilderState = value }, ()))

setEvents :: [] Data.Text.Text -> UpdateWebhookInputBuilder ()
setEvents value =
   UpdateWebhookInputBuilder (\s -> (s { eventsBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> UpdateWebhookInputBuilder ()
setChangeReason value =
   UpdateWebhookInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: UpdateWebhookInputBuilder () -> Data.Either.Either Data.Text.Text UpdateWebhookInput
build builder = do
    let (st, _) = runUpdateWebhookInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWebhookInput.UpdateWebhookInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWebhookInput.UpdateWebhookInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWebhookInput.UpdateWebhookInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWebhookInput.UpdateWebhookInput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    enabled' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWebhookInput.UpdateWebhookInput.enabled is a required property.") Data.Either.Right (enabledBuilderState st)
    url' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWebhookInput.UpdateWebhookInput.url is a required property.") Data.Either.Right (urlBuilderState st)
    method' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWebhookInput.UpdateWebhookInput.method is a required property.") Data.Either.Right (methodBuilderState st)
    version' <- Data.Either.Right (versionBuilderState st)
    custom_headers' <- Data.Either.Right (custom_headersBuilderState st)
    events' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWebhookInput.UpdateWebhookInput.events is a required property.") Data.Either.Right (eventsBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateWebhookInput.UpdateWebhookInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (UpdateWebhookInput { 
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



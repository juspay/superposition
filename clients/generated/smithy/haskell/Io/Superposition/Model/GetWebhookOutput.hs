module Io.Superposition.Model.GetWebhookOutput (
    setName,
    setDescription,
    setEnabled,
    setUrl,
    setMethod,
    setVersion,
    setCustomHeaders,
    setEvents,
    setMaxRetries,
    setLastTriggeredAt,
    setChangeReason,
    setCreatedBy,
    setCreatedAt,
    setLastModifiedBy,
    setLastModifiedAt,
    build,
    GetWebhookOutputBuilder,
    GetWebhookOutput,
    name,
    description,
    enabled,
    url,
    method,
    version,
    custom_headers,
    events,
    max_retries,
    last_triggered_at,
    change_reason,
    created_by,
    created_at,
    last_modified_by,
    last_modified_at
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
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.HttpMethod
import qualified Io.Superposition.Model.Version

data GetWebhookOutput = GetWebhookOutput {
    name :: Data.Text.Text,
    description :: Data.Text.Text,
    enabled :: Bool,
    url :: Data.Text.Text,
    method :: Io.Superposition.Model.HttpMethod.HttpMethod,
    version :: Io.Superposition.Model.Version.Version,
    custom_headers :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    events :: [] Data.Text.Text,
    max_retries :: Integer,
    last_triggered_at :: Data.Maybe.Maybe Data.Time.UTCTime,
    change_reason :: Data.Text.Text,
    created_by :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetWebhookOutput where
    toJSON a = Data.Aeson.object [
        "name" Data.Aeson..= name a,
        "description" Data.Aeson..= description a,
        "enabled" Data.Aeson..= enabled a,
        "url" Data.Aeson..= url a,
        "method" Data.Aeson..= method a,
        "version" Data.Aeson..= version a,
        "custom_headers" Data.Aeson..= custom_headers a,
        "events" Data.Aeson..= events a,
        "max_retries" Data.Aeson..= max_retries a,
        "last_triggered_at" Data.Aeson..= last_triggered_at a,
        "change_reason" Data.Aeson..= change_reason a,
        "created_by" Data.Aeson..= created_by a,
        "created_at" Data.Aeson..= created_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "last_modified_at" Data.Aeson..= last_modified_at a
        ]
    


instance Data.Aeson.FromJSON GetWebhookOutput where
    parseJSON = Data.Aeson.withObject "GetWebhookOutput" $ \v -> GetWebhookOutput
        Data.Functor.<$> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "enabled")
        Control.Applicative.<*> (v Data.Aeson..: "url")
        Control.Applicative.<*> (v Data.Aeson..: "method")
        Control.Applicative.<*> (v Data.Aeson..: "version")
        Control.Applicative.<*> (v Data.Aeson..: "custom_headers")
        Control.Applicative.<*> (v Data.Aeson..: "events")
        Control.Applicative.<*> (v Data.Aeson..: "max_retries")
        Control.Applicative.<*> (v Data.Aeson..: "last_triggered_at")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
    



data GetWebhookOutputBuilderState = GetWebhookOutputBuilderState {
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    enabledBuilderState :: Data.Maybe.Maybe Bool,
    urlBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    methodBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.HttpMethod.HttpMethod,
    versionBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.Version.Version,
    custom_headersBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    eventsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    max_retriesBuilderState :: Data.Maybe.Maybe Integer,
    last_triggered_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetWebhookOutputBuilderState
defaultBuilderState = GetWebhookOutputBuilderState {
    nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    enabledBuilderState = Data.Maybe.Nothing,
    urlBuilderState = Data.Maybe.Nothing,
    methodBuilderState = Data.Maybe.Nothing,
    versionBuilderState = Data.Maybe.Nothing,
    custom_headersBuilderState = Data.Maybe.Nothing,
    eventsBuilderState = Data.Maybe.Nothing,
    max_retriesBuilderState = Data.Maybe.Nothing,
    last_triggered_atBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing
}

newtype GetWebhookOutputBuilder a = GetWebhookOutputBuilder {
    runGetWebhookOutputBuilder :: GetWebhookOutputBuilderState -> (GetWebhookOutputBuilderState, a)
}

instance Data.Functor.Functor GetWebhookOutputBuilder where
    fmap f (GetWebhookOutputBuilder g) =
        GetWebhookOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetWebhookOutputBuilder where
    pure a = GetWebhookOutputBuilder (\s -> (s, a))
    (GetWebhookOutputBuilder f) <*> (GetWebhookOutputBuilder g) = GetWebhookOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetWebhookOutputBuilder where
    (GetWebhookOutputBuilder f) >>= g = GetWebhookOutputBuilder (\s ->
        let (s', a) = f s
            (GetWebhookOutputBuilder h) = g a
        in h s')

setName :: Data.Text.Text -> GetWebhookOutputBuilder ()
setName value =
   GetWebhookOutputBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> GetWebhookOutputBuilder ()
setDescription value =
   GetWebhookOutputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setEnabled :: Bool -> GetWebhookOutputBuilder ()
setEnabled value =
   GetWebhookOutputBuilder (\s -> (s { enabledBuilderState = Data.Maybe.Just value }, ()))

setUrl :: Data.Text.Text -> GetWebhookOutputBuilder ()
setUrl value =
   GetWebhookOutputBuilder (\s -> (s { urlBuilderState = Data.Maybe.Just value }, ()))

setMethod :: Io.Superposition.Model.HttpMethod.HttpMethod -> GetWebhookOutputBuilder ()
setMethod value =
   GetWebhookOutputBuilder (\s -> (s { methodBuilderState = Data.Maybe.Just value }, ()))

setVersion :: Io.Superposition.Model.Version.Version -> GetWebhookOutputBuilder ()
setVersion value =
   GetWebhookOutputBuilder (\s -> (s { versionBuilderState = Data.Maybe.Just value }, ()))

setCustomHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> GetWebhookOutputBuilder ()
setCustomHeaders value =
   GetWebhookOutputBuilder (\s -> (s { custom_headersBuilderState = value }, ()))

setEvents :: [] Data.Text.Text -> GetWebhookOutputBuilder ()
setEvents value =
   GetWebhookOutputBuilder (\s -> (s { eventsBuilderState = Data.Maybe.Just value }, ()))

setMaxRetries :: Integer -> GetWebhookOutputBuilder ()
setMaxRetries value =
   GetWebhookOutputBuilder (\s -> (s { max_retriesBuilderState = Data.Maybe.Just value }, ()))

setLastTriggeredAt :: Data.Maybe.Maybe Data.Time.UTCTime -> GetWebhookOutputBuilder ()
setLastTriggeredAt value =
   GetWebhookOutputBuilder (\s -> (s { last_triggered_atBuilderState = value }, ()))

setChangeReason :: Data.Text.Text -> GetWebhookOutputBuilder ()
setChangeReason value =
   GetWebhookOutputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setCreatedBy :: Data.Text.Text -> GetWebhookOutputBuilder ()
setCreatedBy value =
   GetWebhookOutputBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setCreatedAt :: Data.Time.UTCTime -> GetWebhookOutputBuilder ()
setCreatedAt value =
   GetWebhookOutputBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> GetWebhookOutputBuilder ()
setLastModifiedBy value =
   GetWebhookOutputBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedAt :: Data.Time.UTCTime -> GetWebhookOutputBuilder ()
setLastModifiedAt value =
   GetWebhookOutputBuilder (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }, ()))

build :: GetWebhookOutputBuilder () -> Data.Either.Either Data.Text.Text GetWebhookOutput
build builder = do
    let (st, _) = runGetWebhookOutputBuilder builder defaultBuilderState
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.name is a required property.") Data.Either.Right (nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    enabled' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.enabled is a required property.") Data.Either.Right (enabledBuilderState st)
    url' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.url is a required property.") Data.Either.Right (urlBuilderState st)
    method' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.method is a required property.") Data.Either.Right (methodBuilderState st)
    version' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.version is a required property.") Data.Either.Right (versionBuilderState st)
    custom_headers' <- Data.Either.Right (custom_headersBuilderState st)
    events' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.events is a required property.") Data.Either.Right (eventsBuilderState st)
    max_retries' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.max_retries is a required property.") Data.Either.Right (max_retriesBuilderState st)
    last_triggered_at' <- Data.Either.Right (last_triggered_atBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    Data.Either.Right (GetWebhookOutput { 
        name = name',
        description = description',
        enabled = enabled',
        url = url',
        method = method',
        version = version',
        custom_headers = custom_headers',
        events = events',
        max_retries = max_retries',
        last_triggered_at = last_triggered_at',
        change_reason = change_reason',
        created_by = created_by',
        created_at = created_at',
        last_modified_by = last_modified_by',
        last_modified_at = last_modified_at'
    })



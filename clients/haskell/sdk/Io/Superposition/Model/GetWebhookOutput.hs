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
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.HttpMethod
import qualified Io.Superposition.Model.Version
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data GetWebhookOutput = GetWebhookOutput {
    name :: Data.Text.Text,
    description :: Data.Text.Text,
    enabled :: Bool,
    url :: Data.Text.Text,
    method :: Io.Superposition.Model.HttpMethod.HttpMethod,
    version :: Io.Superposition.Model.Version.Version,
    custom_headers :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    events :: [] Data.Text.Text,
    max_retries :: Data.Int.Int32,
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
    

instance Io.Superposition.Utility.SerializeBody GetWebhookOutput

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
    max_retriesBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
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

type GetWebhookOutputBuilder = Control.Monad.State.Strict.State GetWebhookOutputBuilderState

setName :: Data.Text.Text -> GetWebhookOutputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> GetWebhookOutputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setEnabled :: Bool -> GetWebhookOutputBuilder ()
setEnabled value =
   Control.Monad.State.Strict.modify (\s -> (s { enabledBuilderState = Data.Maybe.Just value }))

setUrl :: Data.Text.Text -> GetWebhookOutputBuilder ()
setUrl value =
   Control.Monad.State.Strict.modify (\s -> (s { urlBuilderState = Data.Maybe.Just value }))

setMethod :: Io.Superposition.Model.HttpMethod.HttpMethod -> GetWebhookOutputBuilder ()
setMethod value =
   Control.Monad.State.Strict.modify (\s -> (s { methodBuilderState = Data.Maybe.Just value }))

setVersion :: Io.Superposition.Model.Version.Version -> GetWebhookOutputBuilder ()
setVersion value =
   Control.Monad.State.Strict.modify (\s -> (s { versionBuilderState = Data.Maybe.Just value }))

setCustomHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> GetWebhookOutputBuilder ()
setCustomHeaders value =
   Control.Monad.State.Strict.modify (\s -> (s { custom_headersBuilderState = value }))

setEvents :: [] Data.Text.Text -> GetWebhookOutputBuilder ()
setEvents value =
   Control.Monad.State.Strict.modify (\s -> (s { eventsBuilderState = Data.Maybe.Just value }))

setMaxRetries :: Data.Int.Int32 -> GetWebhookOutputBuilder ()
setMaxRetries value =
   Control.Monad.State.Strict.modify (\s -> (s { max_retriesBuilderState = Data.Maybe.Just value }))

setLastTriggeredAt :: Data.Maybe.Maybe Data.Time.UTCTime -> GetWebhookOutputBuilder ()
setLastTriggeredAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_triggered_atBuilderState = value }))

setChangeReason :: Data.Text.Text -> GetWebhookOutputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> GetWebhookOutputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> GetWebhookOutputBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> GetWebhookOutputBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> GetWebhookOutputBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

build :: GetWebhookOutputBuilder () -> Data.Either.Either Data.Text.Text GetWebhookOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
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


instance Io.Superposition.Utility.FromResponseParser GetWebhookOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "method"
        var1 <- Io.Superposition.Utility.deSerField "max_retries"
        var2 <- Io.Superposition.Utility.deSerField "description"
        var3 <- Io.Superposition.Utility.deSerField "created_at"
        var4 <- Io.Superposition.Utility.deSerField "last_modified_by"
        var5 <- Io.Superposition.Utility.deSerField "version"
        var6 <- Io.Superposition.Utility.deSerField "created_by"
        var7 <- Io.Superposition.Utility.deSerField "enabled"
        var8 <- Io.Superposition.Utility.deSerField "url"
        var9 <- Io.Superposition.Utility.deSerField "last_modified_at"
        var10 <- Io.Superposition.Utility.deSerField "change_reason"
        var11 <- Io.Superposition.Utility.deSerField "last_triggered_at"
        var12 <- Io.Superposition.Utility.deSerField "name"
        var13 <- Io.Superposition.Utility.deSerField "events"
        var14 <- Io.Superposition.Utility.deSerField "custom_headers"
        pure $ GetWebhookOutput {
            name = var12,
            description = var2,
            enabled = var7,
            url = var8,
            method = var0,
            version = var5,
            custom_headers = var14,
            events = var13,
            max_retries = var1,
            last_triggered_at = var11,
            change_reason = var10,
            created_by = var6,
            created_at = var3,
            last_modified_by = var4,
            last_modified_at = var9
        }


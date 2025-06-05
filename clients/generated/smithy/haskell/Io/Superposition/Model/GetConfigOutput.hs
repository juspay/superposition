module Io.Superposition.Model.GetConfigOutput (
    setContexts,
    setOverrides,
    setDefaultConfigs,
    setVersion,
    setLastModified,
    setAuditId,
    build,
    GetConfigOutputBuilder,
    GetConfigOutput,
    contexts,
    overrides,
    default_configs,
    version,
    last_modified,
    audit_id
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
import qualified Io.Superposition.Model.ContextPartial

data GetConfigOutput = GetConfigOutput {
    contexts :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextPartial.ContextPartial),
    overrides :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text (Data.Map.Map Data.Text.Text Data.Aeson.Value)),
    default_configs :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    version :: Data.Maybe.Maybe Data.Text.Text,
    last_modified :: Data.Maybe.Maybe Data.Time.UTCTime,
    audit_id :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetConfigOutput where
    toJSON a = Data.Aeson.object [
        "contexts" Data.Aeson..= contexts a,
        "overrides" Data.Aeson..= overrides a,
        "default_configs" Data.Aeson..= default_configs a,
        "version" Data.Aeson..= version a,
        "last_modified" Data.Aeson..= last_modified a,
        "audit_id" Data.Aeson..= audit_id a
        ]
    


instance Data.Aeson.FromJSON GetConfigOutput where
    parseJSON = Data.Aeson.withObject "GetConfigOutput" $ \v -> GetConfigOutput
        Data.Functor.<$> (v Data.Aeson..: "contexts")
        Control.Applicative.<*> (v Data.Aeson..: "overrides")
        Control.Applicative.<*> (v Data.Aeson..: "default_configs")
        Control.Applicative.<*> (v Data.Aeson..: "version")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified")
        Control.Applicative.<*> (v Data.Aeson..: "audit_id")
    



data GetConfigOutputBuilderState = GetConfigOutputBuilderState {
    contextsBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextPartial.ContextPartial),
    overridesBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text (Data.Map.Map Data.Text.Text Data.Aeson.Value)),
    default_configsBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    audit_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetConfigOutputBuilderState
defaultBuilderState = GetConfigOutputBuilderState {
    contextsBuilderState = Data.Maybe.Nothing,
    overridesBuilderState = Data.Maybe.Nothing,
    default_configsBuilderState = Data.Maybe.Nothing,
    versionBuilderState = Data.Maybe.Nothing,
    last_modifiedBuilderState = Data.Maybe.Nothing,
    audit_idBuilderState = Data.Maybe.Nothing
}

newtype GetConfigOutputBuilder a = GetConfigOutputBuilder {
    runGetConfigOutputBuilder :: GetConfigOutputBuilderState -> (GetConfigOutputBuilderState, a)
}

instance Data.Functor.Functor GetConfigOutputBuilder where
    fmap f (GetConfigOutputBuilder g) =
        GetConfigOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetConfigOutputBuilder where
    pure a = GetConfigOutputBuilder (\s -> (s, a))
    (GetConfigOutputBuilder f) <*> (GetConfigOutputBuilder g) = GetConfigOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetConfigOutputBuilder where
    (GetConfigOutputBuilder f) >>= g = GetConfigOutputBuilder (\s ->
        let (s', a) = f s
            (GetConfigOutputBuilder h) = g a
        in h s')

setContexts :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextPartial.ContextPartial) -> GetConfigOutputBuilder ()
setContexts value =
   GetConfigOutputBuilder (\s -> (s { contextsBuilderState = value }, ()))

setOverrides :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text (Data.Map.Map Data.Text.Text Data.Aeson.Value)) -> GetConfigOutputBuilder ()
setOverrides value =
   GetConfigOutputBuilder (\s -> (s { overridesBuilderState = value }, ()))

setDefaultConfigs :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> GetConfigOutputBuilder ()
setDefaultConfigs value =
   GetConfigOutputBuilder (\s -> (s { default_configsBuilderState = value }, ()))

setVersion :: Data.Maybe.Maybe Data.Text.Text -> GetConfigOutputBuilder ()
setVersion value =
   GetConfigOutputBuilder (\s -> (s { versionBuilderState = value }, ()))

setLastModified :: Data.Maybe.Maybe Data.Time.UTCTime -> GetConfigOutputBuilder ()
setLastModified value =
   GetConfigOutputBuilder (\s -> (s { last_modifiedBuilderState = value }, ()))

setAuditId :: Data.Maybe.Maybe Data.Text.Text -> GetConfigOutputBuilder ()
setAuditId value =
   GetConfigOutputBuilder (\s -> (s { audit_idBuilderState = value }, ()))

build :: GetConfigOutputBuilder () -> Data.Either.Either Data.Text.Text GetConfigOutput
build builder = do
    let (st, _) = runGetConfigOutputBuilder builder defaultBuilderState
    contexts' <- Data.Either.Right (contextsBuilderState st)
    overrides' <- Data.Either.Right (overridesBuilderState st)
    default_configs' <- Data.Either.Right (default_configsBuilderState st)
    version' <- Data.Either.Right (versionBuilderState st)
    last_modified' <- Data.Either.Right (last_modifiedBuilderState st)
    audit_id' <- Data.Either.Right (audit_idBuilderState st)
    Data.Either.Right (GetConfigOutput { 
        contexts = contexts',
        overrides = overrides',
        default_configs = default_configs',
        version = version',
        last_modified = last_modified',
        audit_id = audit_id'
    })



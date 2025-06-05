module Io.Superposition.Model.GetConfigFastOutput (
    setConfig,
    setVersion,
    setLastModified,
    setAuditId,
    build,
    GetConfigFastOutputBuilder,
    GetConfigFastOutput,
    config,
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
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show

data GetConfigFastOutput = GetConfigFastOutput {
    config :: Data.Maybe.Maybe Data.Aeson.Value,
    version :: Data.Maybe.Maybe Data.Text.Text,
    last_modified :: Data.Maybe.Maybe Data.Time.UTCTime,
    audit_id :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetConfigFastOutput where
    toJSON a = Data.Aeson.object [
        "config" Data.Aeson..= config a,
        "version" Data.Aeson..= version a,
        "last_modified" Data.Aeson..= last_modified a,
        "audit_id" Data.Aeson..= audit_id a
        ]
    


instance Data.Aeson.FromJSON GetConfigFastOutput where
    parseJSON = Data.Aeson.withObject "GetConfigFastOutput" $ \v -> GetConfigFastOutput
        Data.Functor.<$> (v Data.Aeson..: "config")
        Control.Applicative.<*> (v Data.Aeson..: "version")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified")
        Control.Applicative.<*> (v Data.Aeson..: "audit_id")
    



data GetConfigFastOutputBuilderState = GetConfigFastOutputBuilderState {
    configBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    audit_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetConfigFastOutputBuilderState
defaultBuilderState = GetConfigFastOutputBuilderState {
    configBuilderState = Data.Maybe.Nothing,
    versionBuilderState = Data.Maybe.Nothing,
    last_modifiedBuilderState = Data.Maybe.Nothing,
    audit_idBuilderState = Data.Maybe.Nothing
}

newtype GetConfigFastOutputBuilder a = GetConfigFastOutputBuilder {
    runGetConfigFastOutputBuilder :: GetConfigFastOutputBuilderState -> (GetConfigFastOutputBuilderState, a)
}

instance Data.Functor.Functor GetConfigFastOutputBuilder where
    fmap f (GetConfigFastOutputBuilder g) =
        GetConfigFastOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetConfigFastOutputBuilder where
    pure a = GetConfigFastOutputBuilder (\s -> (s, a))
    (GetConfigFastOutputBuilder f) <*> (GetConfigFastOutputBuilder g) = GetConfigFastOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetConfigFastOutputBuilder where
    (GetConfigFastOutputBuilder f) >>= g = GetConfigFastOutputBuilder (\s ->
        let (s', a) = f s
            (GetConfigFastOutputBuilder h) = g a
        in h s')

setConfig :: Data.Maybe.Maybe Data.Aeson.Value -> GetConfigFastOutputBuilder ()
setConfig value =
   GetConfigFastOutputBuilder (\s -> (s { configBuilderState = value }, ()))

setVersion :: Data.Maybe.Maybe Data.Text.Text -> GetConfigFastOutputBuilder ()
setVersion value =
   GetConfigFastOutputBuilder (\s -> (s { versionBuilderState = value }, ()))

setLastModified :: Data.Maybe.Maybe Data.Time.UTCTime -> GetConfigFastOutputBuilder ()
setLastModified value =
   GetConfigFastOutputBuilder (\s -> (s { last_modifiedBuilderState = value }, ()))

setAuditId :: Data.Maybe.Maybe Data.Text.Text -> GetConfigFastOutputBuilder ()
setAuditId value =
   GetConfigFastOutputBuilder (\s -> (s { audit_idBuilderState = value }, ()))

build :: GetConfigFastOutputBuilder () -> Data.Either.Either Data.Text.Text GetConfigFastOutput
build builder = do
    let (st, _) = runGetConfigFastOutputBuilder builder defaultBuilderState
    config' <- Data.Either.Right (configBuilderState st)
    version' <- Data.Either.Right (versionBuilderState st)
    last_modified' <- Data.Either.Right (last_modifiedBuilderState st)
    audit_id' <- Data.Either.Right (audit_idBuilderState st)
    Data.Either.Right (GetConfigFastOutput { 
        config = config',
        version = version',
        last_modified = last_modified',
        audit_id = audit_id'
    })



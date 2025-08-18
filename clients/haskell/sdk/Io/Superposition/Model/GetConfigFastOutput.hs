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
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

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
    

instance Io.Superposition.Utility.SerializeBody GetConfigFastOutput

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

type GetConfigFastOutputBuilder = Control.Monad.State.Strict.State GetConfigFastOutputBuilderState

setConfig :: Data.Maybe.Maybe Data.Aeson.Value -> GetConfigFastOutputBuilder ()
setConfig value =
   Control.Monad.State.Strict.modify (\s -> (s { configBuilderState = value }))

setVersion :: Data.Maybe.Maybe Data.Text.Text -> GetConfigFastOutputBuilder ()
setVersion value =
   Control.Monad.State.Strict.modify (\s -> (s { versionBuilderState = value }))

setLastModified :: Data.Maybe.Maybe Data.Time.UTCTime -> GetConfigFastOutputBuilder ()
setLastModified value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modifiedBuilderState = value }))

setAuditId :: Data.Maybe.Maybe Data.Text.Text -> GetConfigFastOutputBuilder ()
setAuditId value =
   Control.Monad.State.Strict.modify (\s -> (s { audit_idBuilderState = value }))

build :: GetConfigFastOutputBuilder () -> Data.Either.Either Data.Text.Text GetConfigFastOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
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


instance Io.Superposition.Utility.FromResponseParser GetConfigFastOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        var0 <- Io.Superposition.Utility.deSerHeader "x-audit-id"
        var1 <- Io.Superposition.Utility.deSerHeader "x-config-version"
        var2 <- Io.Superposition.Utility.deSerHeader "last-modified"
        var3 <- Io.Superposition.Utility.deSerBody
        pure $ GetConfigFastOutput {
            config = var3,
            version = var1,
            last_modified = var2,
            audit_id = var0
        }


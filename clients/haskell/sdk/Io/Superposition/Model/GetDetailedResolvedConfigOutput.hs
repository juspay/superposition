module Io.Superposition.Model.GetDetailedResolvedConfigOutput (
    setConfig,
    setVersion,
    setLastModified,
    setAuditId,
    build,
    GetDetailedResolvedConfigOutputBuilder,
    GetDetailedResolvedConfigOutput,
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

data GetDetailedResolvedConfigOutput = GetDetailedResolvedConfigOutput {
    config :: Data.Aeson.Value,
    version :: Data.Text.Text,
    last_modified :: Data.Time.UTCTime,
    audit_id :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetDetailedResolvedConfigOutput where
    toJSON a = Data.Aeson.object [
        "config" Data.Aeson..= config a,
        "version" Data.Aeson..= version a,
        "last_modified" Data.Aeson..= last_modified a,
        "audit_id" Data.Aeson..= audit_id a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetDetailedResolvedConfigOutput

instance Data.Aeson.FromJSON GetDetailedResolvedConfigOutput where
    parseJSON = Data.Aeson.withObject "GetDetailedResolvedConfigOutput" $ \v -> GetDetailedResolvedConfigOutput
        Data.Functor.<$> (v Data.Aeson..: "config")
        Control.Applicative.<*> (v Data.Aeson..: "version")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified")
        Control.Applicative.<*> (v Data.Aeson..:? "audit_id")
    



data GetDetailedResolvedConfigOutputBuilderState = GetDetailedResolvedConfigOutputBuilderState {
    configBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    audit_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetDetailedResolvedConfigOutputBuilderState
defaultBuilderState = GetDetailedResolvedConfigOutputBuilderState {
    configBuilderState = Data.Maybe.Nothing,
    versionBuilderState = Data.Maybe.Nothing,
    last_modifiedBuilderState = Data.Maybe.Nothing,
    audit_idBuilderState = Data.Maybe.Nothing
}

type GetDetailedResolvedConfigOutputBuilder = Control.Monad.State.Strict.State GetDetailedResolvedConfigOutputBuilderState

setConfig :: Data.Aeson.Value -> GetDetailedResolvedConfigOutputBuilder ()
setConfig value =
   Control.Monad.State.Strict.modify (\s -> (s { configBuilderState = Data.Maybe.Just value }))

setVersion :: Data.Text.Text -> GetDetailedResolvedConfigOutputBuilder ()
setVersion value =
   Control.Monad.State.Strict.modify (\s -> (s { versionBuilderState = Data.Maybe.Just value }))

setLastModified :: Data.Time.UTCTime -> GetDetailedResolvedConfigOutputBuilder ()
setLastModified value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modifiedBuilderState = Data.Maybe.Just value }))

setAuditId :: Data.Maybe.Maybe Data.Text.Text -> GetDetailedResolvedConfigOutputBuilder ()
setAuditId value =
   Control.Monad.State.Strict.modify (\s -> (s { audit_idBuilderState = value }))

build :: GetDetailedResolvedConfigOutputBuilder () -> Data.Either.Either Data.Text.Text GetDetailedResolvedConfigOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    config' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDetailedResolvedConfigOutput.GetDetailedResolvedConfigOutput.config is a required property.") Data.Either.Right (configBuilderState st)
    version' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDetailedResolvedConfigOutput.GetDetailedResolvedConfigOutput.version is a required property.") Data.Either.Right (versionBuilderState st)
    last_modified' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDetailedResolvedConfigOutput.GetDetailedResolvedConfigOutput.last_modified is a required property.") Data.Either.Right (last_modifiedBuilderState st)
    audit_id' <- Data.Either.Right (audit_idBuilderState st)
    Data.Either.Right (GetDetailedResolvedConfigOutput { 
        config = config',
        version = version',
        last_modified = last_modified',
        audit_id = audit_id'
    })


instance Io.Superposition.Utility.FromResponseParser GetDetailedResolvedConfigOutput where
    expectedStatus = (Network.HTTP.Types.mkStatus 200 "")
    responseParser = do
        var0 <- Io.Superposition.Utility.deSerHeader "x-audit-id"
        var1 <- Io.Superposition.Utility.deSerHeader "x-config-version"
        var2 <- Io.Superposition.Utility.deSerHeader "last-modified"
        var3 <- Io.Superposition.Utility.deSerBody
        pure $ GetDetailedResolvedConfigOutput {
            config = var3,
            version = var1,
            last_modified = var2,
            audit_id = var0
        }


module Io.Superposition.Model.GetResolvedConfigExplanationOutput (
    setExplanation,
    setVersion,
    setLastModified,
    setAuditId,
    build,
    GetResolvedConfigExplanationOutputBuilder,
    GetResolvedConfigExplanationOutput,
    explanation,
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
import qualified Io.Superposition.Model.ResolveExplanation
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data GetResolvedConfigExplanationOutput = GetResolvedConfigExplanationOutput {
    explanation :: Io.Superposition.Model.ResolveExplanation.ResolveExplanation,
    version :: Data.Text.Text,
    last_modified :: Data.Time.UTCTime,
    audit_id :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetResolvedConfigExplanationOutput where
    toJSON a = Data.Aeson.object [
        "explanation" Data.Aeson..= explanation a,
        "version" Data.Aeson..= version a,
        "last_modified" Data.Aeson..= last_modified a,
        "audit_id" Data.Aeson..= audit_id a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetResolvedConfigExplanationOutput

instance Data.Aeson.FromJSON GetResolvedConfigExplanationOutput where
    parseJSON = Data.Aeson.withObject "GetResolvedConfigExplanationOutput" $ \v -> GetResolvedConfigExplanationOutput
        Data.Functor.<$> (v Data.Aeson..: "explanation")
        Control.Applicative.<*> (v Data.Aeson..: "version")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified")
        Control.Applicative.<*> (v Data.Aeson..:? "audit_id")
    



data GetResolvedConfigExplanationOutputBuilderState = GetResolvedConfigExplanationOutputBuilderState {
    explanationBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ResolveExplanation.ResolveExplanation,
    versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    audit_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetResolvedConfigExplanationOutputBuilderState
defaultBuilderState = GetResolvedConfigExplanationOutputBuilderState {
    explanationBuilderState = Data.Maybe.Nothing,
    versionBuilderState = Data.Maybe.Nothing,
    last_modifiedBuilderState = Data.Maybe.Nothing,
    audit_idBuilderState = Data.Maybe.Nothing
}

type GetResolvedConfigExplanationOutputBuilder = Control.Monad.State.Strict.State GetResolvedConfigExplanationOutputBuilderState

setExplanation :: Io.Superposition.Model.ResolveExplanation.ResolveExplanation -> GetResolvedConfigExplanationOutputBuilder ()
setExplanation value =
   Control.Monad.State.Strict.modify (\s -> (s { explanationBuilderState = Data.Maybe.Just value }))

setVersion :: Data.Text.Text -> GetResolvedConfigExplanationOutputBuilder ()
setVersion value =
   Control.Monad.State.Strict.modify (\s -> (s { versionBuilderState = Data.Maybe.Just value }))

setLastModified :: Data.Time.UTCTime -> GetResolvedConfigExplanationOutputBuilder ()
setLastModified value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modifiedBuilderState = Data.Maybe.Just value }))

setAuditId :: Data.Maybe.Maybe Data.Text.Text -> GetResolvedConfigExplanationOutputBuilder ()
setAuditId value =
   Control.Monad.State.Strict.modify (\s -> (s { audit_idBuilderState = value }))

build :: GetResolvedConfigExplanationOutputBuilder () -> Data.Either.Either Data.Text.Text GetResolvedConfigExplanationOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    explanation' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetResolvedConfigExplanationOutput.GetResolvedConfigExplanationOutput.explanation is a required property.") Data.Either.Right (explanationBuilderState st)
    version' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetResolvedConfigExplanationOutput.GetResolvedConfigExplanationOutput.version is a required property.") Data.Either.Right (versionBuilderState st)
    last_modified' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetResolvedConfigExplanationOutput.GetResolvedConfigExplanationOutput.last_modified is a required property.") Data.Either.Right (last_modifiedBuilderState st)
    audit_id' <- Data.Either.Right (audit_idBuilderState st)
    Data.Either.Right (GetResolvedConfigExplanationOutput { 
        explanation = explanation',
        version = version',
        last_modified = last_modified',
        audit_id = audit_id'
    })


instance Io.Superposition.Utility.FromResponseParser GetResolvedConfigExplanationOutput where
    expectedStatus = (Network.HTTP.Types.mkStatus 200 "")
    responseParser = do
        var0 <- Io.Superposition.Utility.deSerHeader "x-audit-id"
        var1 <- Io.Superposition.Utility.deSerHeader "x-config-version"
        var2 <- Io.Superposition.Utility.deSerHeader "last-modified"
        var3 <- Io.Superposition.Utility.deSerBody
        pure $ GetResolvedConfigExplanationOutput {
            explanation = var3,
            version = var1,
            last_modified = var2,
            audit_id = var0
        }


module Io.Superposition.Model.GetExperimentConfigInput (
    setWorkspaceId,
    setOrgId,
    setIfModifiedSince,
    setPrefix,
    setContext,
    build,
    GetExperimentConfigInputBuilder,
    GetExperimentConfigInput,
    workspace_id,
    org_id,
    if_modified_since,
    prefix,
    context
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
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data GetExperimentConfigInput = GetExperimentConfigInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    if_modified_since :: Data.Maybe.Maybe Data.Time.UTCTime,
    prefix :: Data.Maybe.Maybe ([] Data.Text.Text),
    context :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetExperimentConfigInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "if_modified_since" Data.Aeson..= if_modified_since a,
        "prefix" Data.Aeson..= prefix a,
        "context" Data.Aeson..= context a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetExperimentConfigInput

instance Data.Aeson.FromJSON GetExperimentConfigInput where
    parseJSON = Data.Aeson.withObject "GetExperimentConfigInput" $ \v -> GetExperimentConfigInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "if_modified_since")
        Control.Applicative.<*> (v Data.Aeson..:? "prefix")
        Control.Applicative.<*> (v Data.Aeson..:? "context")
    



data GetExperimentConfigInputBuilderState = GetExperimentConfigInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    if_modified_sinceBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    prefixBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetExperimentConfigInputBuilderState
defaultBuilderState = GetExperimentConfigInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    if_modified_sinceBuilderState = Data.Maybe.Nothing,
    prefixBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing
}

type GetExperimentConfigInputBuilder = Control.Monad.State.Strict.State GetExperimentConfigInputBuilderState

setWorkspaceId :: Data.Text.Text -> GetExperimentConfigInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> GetExperimentConfigInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setIfModifiedSince :: Data.Maybe.Maybe Data.Time.UTCTime -> GetExperimentConfigInputBuilder ()
setIfModifiedSince value =
   Control.Monad.State.Strict.modify (\s -> (s { if_modified_sinceBuilderState = value }))

setPrefix :: Data.Maybe.Maybe ([] Data.Text.Text) -> GetExperimentConfigInputBuilder ()
setPrefix value =
   Control.Monad.State.Strict.modify (\s -> (s { prefixBuilderState = value }))

setContext :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> GetExperimentConfigInputBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = value }))

build :: GetExperimentConfigInputBuilder () -> Data.Either.Either Data.Text.Text GetExperimentConfigInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetExperimentConfigInput.GetExperimentConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetExperimentConfigInput.GetExperimentConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    if_modified_since' <- Data.Either.Right (if_modified_sinceBuilderState st)
    prefix' <- Data.Either.Right (prefixBuilderState st)
    context' <- Data.Either.Right (contextBuilderState st)
    Data.Either.Right (GetExperimentConfigInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        if_modified_since = if_modified_since',
        prefix = prefix',
        context = context'
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetExperimentConfigInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "experiment-config"
            ]
        Io.Superposition.Utility.serQuery "prefix" (prefix self)
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "if-modified-since" (if_modified_since self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "context" (context self)


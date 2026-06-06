module Io.Superposition.Model.GetDetailedResolvedConfigInput (
    setWorkspaceId,
    setOrgId,
    setPrefix,
    setVersion,
    setShowReasoning,
    setMergeStrategy,
    setContextId,
    setResolveRemote,
    setContext,
    build,
    GetDetailedResolvedConfigInputBuilder,
    GetDetailedResolvedConfigInput,
    workspace_id,
    org_id,
    prefix,
    version,
    show_reasoning,
    merge_strategy,
    context_id,
    resolve_remote,
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.MergeStrategy
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data GetDetailedResolvedConfigInput = GetDetailedResolvedConfigInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    prefix :: Data.Maybe.Maybe ([] Data.Text.Text),
    version :: Data.Maybe.Maybe Data.Text.Text,
    show_reasoning :: Data.Maybe.Maybe Bool,
    merge_strategy :: Data.Maybe.Maybe Io.Superposition.Model.MergeStrategy.MergeStrategy,
    context_id :: Data.Maybe.Maybe Data.Text.Text,
    resolve_remote :: Data.Maybe.Maybe Bool,
    context :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetDetailedResolvedConfigInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "prefix" Data.Aeson..= prefix a,
        "version" Data.Aeson..= version a,
        "show_reasoning" Data.Aeson..= show_reasoning a,
        "merge_strategy" Data.Aeson..= merge_strategy a,
        "context_id" Data.Aeson..= context_id a,
        "resolve_remote" Data.Aeson..= resolve_remote a,
        "context" Data.Aeson..= context a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetDetailedResolvedConfigInput

instance Data.Aeson.FromJSON GetDetailedResolvedConfigInput where
    parseJSON = Data.Aeson.withObject "GetDetailedResolvedConfigInput" $ \v -> GetDetailedResolvedConfigInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "prefix")
        Control.Applicative.<*> (v Data.Aeson..:? "version")
        Control.Applicative.<*> (v Data.Aeson..:? "show_reasoning")
        Control.Applicative.<*> (v Data.Aeson..:? "merge_strategy")
        Control.Applicative.<*> (v Data.Aeson..:? "context_id")
        Control.Applicative.<*> (v Data.Aeson..:? "resolve_remote")
        Control.Applicative.<*> (v Data.Aeson..:? "context")
    



data GetDetailedResolvedConfigInputBuilderState = GetDetailedResolvedConfigInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    prefixBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    show_reasoningBuilderState :: Data.Maybe.Maybe Bool,
    merge_strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.MergeStrategy.MergeStrategy,
    context_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    resolve_remoteBuilderState :: Data.Maybe.Maybe Bool,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetDetailedResolvedConfigInputBuilderState
defaultBuilderState = GetDetailedResolvedConfigInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    prefixBuilderState = Data.Maybe.Nothing,
    versionBuilderState = Data.Maybe.Nothing,
    show_reasoningBuilderState = Data.Maybe.Nothing,
    merge_strategyBuilderState = Data.Maybe.Nothing,
    context_idBuilderState = Data.Maybe.Nothing,
    resolve_remoteBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing
}

type GetDetailedResolvedConfigInputBuilder = Control.Monad.State.Strict.State GetDetailedResolvedConfigInputBuilderState

setWorkspaceId :: Data.Text.Text -> GetDetailedResolvedConfigInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> GetDetailedResolvedConfigInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setPrefix :: Data.Maybe.Maybe ([] Data.Text.Text) -> GetDetailedResolvedConfigInputBuilder ()
setPrefix value =
   Control.Monad.State.Strict.modify (\s -> (s { prefixBuilderState = value }))

setVersion :: Data.Maybe.Maybe Data.Text.Text -> GetDetailedResolvedConfigInputBuilder ()
setVersion value =
   Control.Monad.State.Strict.modify (\s -> (s { versionBuilderState = value }))

setShowReasoning :: Data.Maybe.Maybe Bool -> GetDetailedResolvedConfigInputBuilder ()
setShowReasoning value =
   Control.Monad.State.Strict.modify (\s -> (s { show_reasoningBuilderState = value }))

setMergeStrategy :: Data.Maybe.Maybe Io.Superposition.Model.MergeStrategy.MergeStrategy -> GetDetailedResolvedConfigInputBuilder ()
setMergeStrategy value =
   Control.Monad.State.Strict.modify (\s -> (s { merge_strategyBuilderState = value }))

setContextId :: Data.Maybe.Maybe Data.Text.Text -> GetDetailedResolvedConfigInputBuilder ()
setContextId value =
   Control.Monad.State.Strict.modify (\s -> (s { context_idBuilderState = value }))

setResolveRemote :: Data.Maybe.Maybe Bool -> GetDetailedResolvedConfigInputBuilder ()
setResolveRemote value =
   Control.Monad.State.Strict.modify (\s -> (s { resolve_remoteBuilderState = value }))

setContext :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> GetDetailedResolvedConfigInputBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = value }))

build :: GetDetailedResolvedConfigInputBuilder () -> Data.Either.Either Data.Text.Text GetDetailedResolvedConfigInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDetailedResolvedConfigInput.GetDetailedResolvedConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDetailedResolvedConfigInput.GetDetailedResolvedConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    prefix' <- Data.Either.Right (prefixBuilderState st)
    version' <- Data.Either.Right (versionBuilderState st)
    show_reasoning' <- Data.Either.Right (show_reasoningBuilderState st)
    merge_strategy' <- Data.Either.Right (merge_strategyBuilderState st)
    context_id' <- Data.Either.Right (context_idBuilderState st)
    resolve_remote' <- Data.Either.Right (resolve_remoteBuilderState st)
    context' <- Data.Either.Right (contextBuilderState st)
    Data.Either.Right (GetDetailedResolvedConfigInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        prefix = prefix',
        version = version',
        show_reasoning = show_reasoning',
        merge_strategy = merge_strategy',
        context_id = context_id',
        resolve_remote = resolve_remote',
        context = context'
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetDetailedResolvedConfigInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "config",
            "resolve",
            "detailed"
            ]
        Io.Superposition.Utility.serQuery "show_reasoning" (show_reasoning self)
        Io.Superposition.Utility.serQuery "prefix" (prefix self)
        Io.Superposition.Utility.serQuery "context_id" (context_id self)
        Io.Superposition.Utility.serQuery "version" (version self)
        Io.Superposition.Utility.serQuery "resolve_remote" (resolve_remote self)
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-merge-strategy" (merge_strategy self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "context" (context self)


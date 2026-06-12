module Io.Superposition.Model.GetResolvedConfigExplanationInput (
    setWorkspaceId,
    setOrgId,
    setKey,
    setVersion,
    setMergeStrategy,
    setContextId,
    setResolveRemote,
    setContext,
    build,
    GetResolvedConfigExplanationInputBuilder,
    GetResolvedConfigExplanationInput,
    workspace_id,
    org_id,
    key,
    version,
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

data GetResolvedConfigExplanationInput = GetResolvedConfigExplanationInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    key :: Data.Text.Text,
    version :: Data.Maybe.Maybe Data.Text.Text,
    merge_strategy :: Data.Maybe.Maybe Io.Superposition.Model.MergeStrategy.MergeStrategy,
    context_id :: Data.Maybe.Maybe Data.Text.Text,
    resolve_remote :: Data.Maybe.Maybe Bool,
    context :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetResolvedConfigExplanationInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "key" Data.Aeson..= key a,
        "version" Data.Aeson..= version a,
        "merge_strategy" Data.Aeson..= merge_strategy a,
        "context_id" Data.Aeson..= context_id a,
        "resolve_remote" Data.Aeson..= resolve_remote a,
        "context" Data.Aeson..= context a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetResolvedConfigExplanationInput

instance Data.Aeson.FromJSON GetResolvedConfigExplanationInput where
    parseJSON = Data.Aeson.withObject "GetResolvedConfigExplanationInput" $ \v -> GetResolvedConfigExplanationInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "key")
        Control.Applicative.<*> (v Data.Aeson..:? "version")
        Control.Applicative.<*> (v Data.Aeson..:? "merge_strategy")
        Control.Applicative.<*> (v Data.Aeson..:? "context_id")
        Control.Applicative.<*> (v Data.Aeson..:? "resolve_remote")
        Control.Applicative.<*> (v Data.Aeson..:? "context")
    



data GetResolvedConfigExplanationInputBuilderState = GetResolvedConfigExplanationInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    merge_strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.MergeStrategy.MergeStrategy,
    context_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    resolve_remoteBuilderState :: Data.Maybe.Maybe Bool,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetResolvedConfigExplanationInputBuilderState
defaultBuilderState = GetResolvedConfigExplanationInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    keyBuilderState = Data.Maybe.Nothing,
    versionBuilderState = Data.Maybe.Nothing,
    merge_strategyBuilderState = Data.Maybe.Nothing,
    context_idBuilderState = Data.Maybe.Nothing,
    resolve_remoteBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing
}

type GetResolvedConfigExplanationInputBuilder = Control.Monad.State.Strict.State GetResolvedConfigExplanationInputBuilderState

setWorkspaceId :: Data.Text.Text -> GetResolvedConfigExplanationInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> GetResolvedConfigExplanationInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setKey :: Data.Text.Text -> GetResolvedConfigExplanationInputBuilder ()
setKey value =
   Control.Monad.State.Strict.modify (\s -> (s { keyBuilderState = Data.Maybe.Just value }))

setVersion :: Data.Maybe.Maybe Data.Text.Text -> GetResolvedConfigExplanationInputBuilder ()
setVersion value =
   Control.Monad.State.Strict.modify (\s -> (s { versionBuilderState = value }))

setMergeStrategy :: Data.Maybe.Maybe Io.Superposition.Model.MergeStrategy.MergeStrategy -> GetResolvedConfigExplanationInputBuilder ()
setMergeStrategy value =
   Control.Monad.State.Strict.modify (\s -> (s { merge_strategyBuilderState = value }))

setContextId :: Data.Maybe.Maybe Data.Text.Text -> GetResolvedConfigExplanationInputBuilder ()
setContextId value =
   Control.Monad.State.Strict.modify (\s -> (s { context_idBuilderState = value }))

setResolveRemote :: Data.Maybe.Maybe Bool -> GetResolvedConfigExplanationInputBuilder ()
setResolveRemote value =
   Control.Monad.State.Strict.modify (\s -> (s { resolve_remoteBuilderState = value }))

setContext :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> GetResolvedConfigExplanationInputBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = value }))

build :: GetResolvedConfigExplanationInputBuilder () -> Data.Either.Either Data.Text.Text GetResolvedConfigExplanationInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetResolvedConfigExplanationInput.GetResolvedConfigExplanationInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetResolvedConfigExplanationInput.GetResolvedConfigExplanationInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetResolvedConfigExplanationInput.GetResolvedConfigExplanationInput.key is a required property.") Data.Either.Right (keyBuilderState st)
    version' <- Data.Either.Right (versionBuilderState st)
    merge_strategy' <- Data.Either.Right (merge_strategyBuilderState st)
    context_id' <- Data.Either.Right (context_idBuilderState st)
    resolve_remote' <- Data.Either.Right (resolve_remoteBuilderState st)
    context' <- Data.Either.Right (contextBuilderState st)
    Data.Either.Right (GetResolvedConfigExplanationInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        key = key',
        version = version',
        merge_strategy = merge_strategy',
        context_id = context_id',
        resolve_remote = resolve_remote',
        context = context'
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetResolvedConfigExplanationInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "config",
            "resolve",
            "explain",
            Io.Superposition.Utility.serializeElement (key self)
            ]
        Io.Superposition.Utility.serQuery "context_id" (context_id self)
        Io.Superposition.Utility.serQuery "version" (version self)
        Io.Superposition.Utility.serQuery "resolve_remote" (resolve_remote self)
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-merge-strategy" (merge_strategy self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "context" (context self)


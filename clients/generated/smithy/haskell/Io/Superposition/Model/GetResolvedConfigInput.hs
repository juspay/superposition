module Io.Superposition.Model.GetResolvedConfigInput (
    setWorkspaceId,
    setOrgId,
    setPrefix,
    setVersion,
    setShowReasoning,
    setMergeStrategy,
    setContextId,
    setContext,
    build,
    GetResolvedConfigInputBuilder,
    GetResolvedConfigInput,
    workspace_id,
    org_id,
    prefix,
    version,
    show_reasoning,
    merge_strategy,
    context_id,
    context
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.MergeStrategy

data GetResolvedConfigInput = GetResolvedConfigInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    prefix :: Data.Maybe.Maybe Data.Text.Text,
    version :: Data.Maybe.Maybe Data.Text.Text,
    show_reasoning :: Data.Maybe.Maybe Bool,
    merge_strategy :: Data.Maybe.Maybe Io.Superposition.Model.MergeStrategy.MergeStrategy,
    context_id :: Data.Maybe.Maybe Data.Text.Text,
    context :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetResolvedConfigInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "prefix" Data.Aeson..= prefix a,
        "version" Data.Aeson..= version a,
        "show_reasoning" Data.Aeson..= show_reasoning a,
        "merge_strategy" Data.Aeson..= merge_strategy a,
        "context_id" Data.Aeson..= context_id a,
        "context" Data.Aeson..= context a
        ]
    


instance Data.Aeson.FromJSON GetResolvedConfigInput where
    parseJSON = Data.Aeson.withObject "GetResolvedConfigInput" $ \v -> GetResolvedConfigInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "prefix")
        Control.Applicative.<*> (v Data.Aeson..: "version")
        Control.Applicative.<*> (v Data.Aeson..: "show_reasoning")
        Control.Applicative.<*> (v Data.Aeson..: "merge_strategy")
        Control.Applicative.<*> (v Data.Aeson..: "context_id")
        Control.Applicative.<*> (v Data.Aeson..: "context")
    



data GetResolvedConfigInputBuilderState = GetResolvedConfigInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    prefixBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    show_reasoningBuilderState :: Data.Maybe.Maybe Bool,
    merge_strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.MergeStrategy.MergeStrategy,
    context_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetResolvedConfigInputBuilderState
defaultBuilderState = GetResolvedConfigInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    prefixBuilderState = Data.Maybe.Nothing,
    versionBuilderState = Data.Maybe.Nothing,
    show_reasoningBuilderState = Data.Maybe.Nothing,
    merge_strategyBuilderState = Data.Maybe.Nothing,
    context_idBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing
}

newtype GetResolvedConfigInputBuilder a = GetResolvedConfigInputBuilder {
    runGetResolvedConfigInputBuilder :: GetResolvedConfigInputBuilderState -> (GetResolvedConfigInputBuilderState, a)
}

instance Data.Functor.Functor GetResolvedConfigInputBuilder where
    fmap f (GetResolvedConfigInputBuilder g) =
        GetResolvedConfigInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetResolvedConfigInputBuilder where
    pure a = GetResolvedConfigInputBuilder (\s -> (s, a))
    (GetResolvedConfigInputBuilder f) <*> (GetResolvedConfigInputBuilder g) = GetResolvedConfigInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetResolvedConfigInputBuilder where
    (GetResolvedConfigInputBuilder f) >>= g = GetResolvedConfigInputBuilder (\s ->
        let (s', a) = f s
            (GetResolvedConfigInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> GetResolvedConfigInputBuilder ()
setWorkspaceId value =
   GetResolvedConfigInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> GetResolvedConfigInputBuilder ()
setOrgId value =
   GetResolvedConfigInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setPrefix :: Data.Maybe.Maybe Data.Text.Text -> GetResolvedConfigInputBuilder ()
setPrefix value =
   GetResolvedConfigInputBuilder (\s -> (s { prefixBuilderState = value }, ()))

setVersion :: Data.Maybe.Maybe Data.Text.Text -> GetResolvedConfigInputBuilder ()
setVersion value =
   GetResolvedConfigInputBuilder (\s -> (s { versionBuilderState = value }, ()))

setShowReasoning :: Data.Maybe.Maybe Bool -> GetResolvedConfigInputBuilder ()
setShowReasoning value =
   GetResolvedConfigInputBuilder (\s -> (s { show_reasoningBuilderState = value }, ()))

setMergeStrategy :: Data.Maybe.Maybe Io.Superposition.Model.MergeStrategy.MergeStrategy -> GetResolvedConfigInputBuilder ()
setMergeStrategy value =
   GetResolvedConfigInputBuilder (\s -> (s { merge_strategyBuilderState = value }, ()))

setContextId :: Data.Maybe.Maybe Data.Text.Text -> GetResolvedConfigInputBuilder ()
setContextId value =
   GetResolvedConfigInputBuilder (\s -> (s { context_idBuilderState = value }, ()))

setContext :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> GetResolvedConfigInputBuilder ()
setContext value =
   GetResolvedConfigInputBuilder (\s -> (s { contextBuilderState = value }, ()))

build :: GetResolvedConfigInputBuilder () -> Data.Either.Either Data.Text.Text GetResolvedConfigInput
build builder = do
    let (st, _) = runGetResolvedConfigInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetResolvedConfigInput.GetResolvedConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetResolvedConfigInput.GetResolvedConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    prefix' <- Data.Either.Right (prefixBuilderState st)
    version' <- Data.Either.Right (versionBuilderState st)
    show_reasoning' <- Data.Either.Right (show_reasoningBuilderState st)
    merge_strategy' <- Data.Either.Right (merge_strategyBuilderState st)
    context_id' <- Data.Either.Right (context_idBuilderState st)
    context' <- Data.Either.Right (contextBuilderState st)
    Data.Either.Right (GetResolvedConfigInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        prefix = prefix',
        version = version',
        show_reasoning = show_reasoning',
        merge_strategy = merge_strategy',
        context_id = context_id',
        context = context'
    })



module Io.Superposition.Model.GetExperimentConfigInput (
    setWorkspaceId,
    setOrgId,
    setIfModifiedSince,
    setPrefix,
    setContext,
<<<<<<< HEAD
    setDimensionMatchStrategy,
=======
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
    build,
    GetExperimentConfigInputBuilder,
    GetExperimentConfigInput,
    workspace_id,
    org_id,
    if_modified_since,
    prefix,
<<<<<<< HEAD
    context,
    dimension_match_strategy
=======
    context
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
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
<<<<<<< HEAD
import qualified Io.Superposition.Model.DimensionMatchStrategy
=======
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data GetExperimentConfigInput = GetExperimentConfigInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    if_modified_since :: Data.Maybe.Maybe Data.Time.UTCTime,
    prefix :: Data.Maybe.Maybe ([] Data.Text.Text),
<<<<<<< HEAD
    context :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    dimension_match_strategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy
=======
    context :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
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
<<<<<<< HEAD
        "context" Data.Aeson..= context a,
        "dimension_match_strategy" Data.Aeson..= dimension_match_strategy a
=======
        "context" Data.Aeson..= context a
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
        ]
    

instance Io.Superposition.Utility.SerializeBody GetExperimentConfigInput

instance Data.Aeson.FromJSON GetExperimentConfigInput where
    parseJSON = Data.Aeson.withObject "GetExperimentConfigInput" $ \v -> GetExperimentConfigInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "if_modified_since")
        Control.Applicative.<*> (v Data.Aeson..:? "prefix")
        Control.Applicative.<*> (v Data.Aeson..:? "context")
<<<<<<< HEAD
        Control.Applicative.<*> (v Data.Aeson..:? "dimension_match_strategy")
=======
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
    



data GetExperimentConfigInputBuilderState = GetExperimentConfigInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    if_modified_sinceBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    prefixBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
<<<<<<< HEAD
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    dimension_match_strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy
=======
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetExperimentConfigInputBuilderState
defaultBuilderState = GetExperimentConfigInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    if_modified_sinceBuilderState = Data.Maybe.Nothing,
    prefixBuilderState = Data.Maybe.Nothing,
<<<<<<< HEAD
    contextBuilderState = Data.Maybe.Nothing,
    dimension_match_strategyBuilderState = Data.Maybe.Nothing
=======
    contextBuilderState = Data.Maybe.Nothing
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
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

<<<<<<< HEAD
setDimensionMatchStrategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy -> GetExperimentConfigInputBuilder ()
setDimensionMatchStrategy value =
   Control.Monad.State.Strict.modify (\s -> (s { dimension_match_strategyBuilderState = value }))

=======
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
build :: GetExperimentConfigInputBuilder () -> Data.Either.Either Data.Text.Text GetExperimentConfigInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetExperimentConfigInput.GetExperimentConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetExperimentConfigInput.GetExperimentConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    if_modified_since' <- Data.Either.Right (if_modified_sinceBuilderState st)
    prefix' <- Data.Either.Right (prefixBuilderState st)
    context' <- Data.Either.Right (contextBuilderState st)
<<<<<<< HEAD
    dimension_match_strategy' <- Data.Either.Right (dimension_match_strategyBuilderState st)
=======
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
    Data.Either.Right (GetExperimentConfigInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        if_modified_since = if_modified_since',
        prefix = prefix',
<<<<<<< HEAD
        context = context',
        dimension_match_strategy = dimension_match_strategy'
=======
        context = context'
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetExperimentConfigInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "experiment-config"
            ]
<<<<<<< HEAD
        Io.Superposition.Utility.serQuery "dimension_match_strategy" (dimension_match_strategy self)
=======
>>>>>>> 0c17747c (feat: Add experiment_config endpoint)
        Io.Superposition.Utility.serQuery "prefix" (prefix self)
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "if-modified-since" (if_modified_since self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "context" (context self)


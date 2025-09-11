module Io.Superposition.Model.ListExperimentInput (
    setWorkspaceId,
    setOrgId,
    setPage,
    setCount,
    setAll',
    setStatus,
    setFromDate,
    setToDate,
    setExperimentName,
    setExperimentIds,
    setExperimentGroupIds,
    setCreatedBy,
    setSortOn,
    setSortBy,
    setGlobalExperimentsOnly,
    setDimensionMatchStrategy,
    build,
    ListExperimentInputBuilder,
    ListExperimentInput,
    workspace_id,
    org_id,
    page,
    count,
    all',
    status,
    from_date,
    to_date,
    experiment_name,
    experiment_ids,
    experiment_group_ids,
    created_by,
    sort_on,
    sort_by,
    global_experiments_only,
    dimension_match_strategy
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.DimensionMatchStrategy
import qualified Io.Superposition.Model.ExperimentSortOn
import qualified Io.Superposition.Model.ExperimentStatusType
import qualified Io.Superposition.Model.SortBy
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ListExperimentInput = ListExperimentInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    page :: Data.Maybe.Maybe Data.Int.Int64,
    count :: Data.Maybe.Maybe Data.Int.Int64,
    all' :: Data.Maybe.Maybe Bool,
    status :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType,
    from_date :: Data.Maybe.Maybe Data.Time.UTCTime,
    to_date :: Data.Maybe.Maybe Data.Time.UTCTime,
    experiment_name :: Data.Maybe.Maybe Data.Text.Text,
    experiment_ids :: Data.Maybe.Maybe Data.Text.Text,
    experiment_group_ids :: Data.Maybe.Maybe Data.Text.Text,
    created_by :: Data.Maybe.Maybe Data.Text.Text,
    sort_on :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentSortOn.ExperimentSortOn,
    sort_by :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
    global_experiments_only :: Data.Maybe.Maybe Bool,
    dimension_match_strategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListExperimentInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "page" Data.Aeson..= page a,
        "count" Data.Aeson..= count a,
        "all" Data.Aeson..= all' a,
        "status" Data.Aeson..= status a,
        "from_date" Data.Aeson..= from_date a,
        "to_date" Data.Aeson..= to_date a,
        "experiment_name" Data.Aeson..= experiment_name a,
        "experiment_ids" Data.Aeson..= experiment_ids a,
        "experiment_group_ids" Data.Aeson..= experiment_group_ids a,
        "created_by" Data.Aeson..= created_by a,
        "sort_on" Data.Aeson..= sort_on a,
        "sort_by" Data.Aeson..= sort_by a,
        "global_experiments_only" Data.Aeson..= global_experiments_only a,
        "dimension_match_strategy" Data.Aeson..= dimension_match_strategy a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListExperimentInput

instance Data.Aeson.FromJSON ListExperimentInput where
    parseJSON = Data.Aeson.withObject "ListExperimentInput" $ \v -> ListExperimentInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "all")
        Control.Applicative.<*> (v Data.Aeson..: "status")
        Control.Applicative.<*> (v Data.Aeson..: "from_date")
        Control.Applicative.<*> (v Data.Aeson..: "to_date")
        Control.Applicative.<*> (v Data.Aeson..: "experiment_name")
        Control.Applicative.<*> (v Data.Aeson..: "experiment_ids")
        Control.Applicative.<*> (v Data.Aeson..: "experiment_group_ids")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "sort_on")
        Control.Applicative.<*> (v Data.Aeson..: "sort_by")
        Control.Applicative.<*> (v Data.Aeson..: "global_experiments_only")
        Control.Applicative.<*> (v Data.Aeson..: "dimension_match_strategy")
    



data ListExperimentInputBuilderState = ListExperimentInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType,
    from_dateBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    to_dateBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    experiment_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    experiment_idsBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    experiment_group_idsBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    sort_onBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentSortOn.ExperimentSortOn,
    sort_byBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
    global_experiments_onlyBuilderState :: Data.Maybe.Maybe Bool,
    dimension_match_strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListExperimentInputBuilderState
defaultBuilderState = ListExperimentInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    countBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    statusBuilderState = Data.Maybe.Nothing,
    from_dateBuilderState = Data.Maybe.Nothing,
    to_dateBuilderState = Data.Maybe.Nothing,
    experiment_nameBuilderState = Data.Maybe.Nothing,
    experiment_idsBuilderState = Data.Maybe.Nothing,
    experiment_group_idsBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    sort_onBuilderState = Data.Maybe.Nothing,
    sort_byBuilderState = Data.Maybe.Nothing,
    global_experiments_onlyBuilderState = Data.Maybe.Nothing,
    dimension_match_strategyBuilderState = Data.Maybe.Nothing
}

type ListExperimentInputBuilder = Control.Monad.State.Strict.State ListExperimentInputBuilderState

setWorkspaceId :: Data.Text.Text -> ListExperimentInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListExperimentInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setPage :: Data.Maybe.Maybe Data.Int.Int64 -> ListExperimentInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setCount :: Data.Maybe.Maybe Data.Int.Int64 -> ListExperimentInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setAll' :: Data.Maybe.Maybe Bool -> ListExperimentInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setStatus :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType -> ListExperimentInputBuilder ()
setStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { statusBuilderState = value }))

setFromDate :: Data.Maybe.Maybe Data.Time.UTCTime -> ListExperimentInputBuilder ()
setFromDate value =
   Control.Monad.State.Strict.modify (\s -> (s { from_dateBuilderState = value }))

setToDate :: Data.Maybe.Maybe Data.Time.UTCTime -> ListExperimentInputBuilder ()
setToDate value =
   Control.Monad.State.Strict.modify (\s -> (s { to_dateBuilderState = value }))

setExperimentName :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentInputBuilder ()
setExperimentName value =
   Control.Monad.State.Strict.modify (\s -> (s { experiment_nameBuilderState = value }))

setExperimentIds :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentInputBuilder ()
setExperimentIds value =
   Control.Monad.State.Strict.modify (\s -> (s { experiment_idsBuilderState = value }))

setExperimentGroupIds :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentInputBuilder ()
setExperimentGroupIds value =
   Control.Monad.State.Strict.modify (\s -> (s { experiment_group_idsBuilderState = value }))

setCreatedBy :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentInputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = value }))

setSortOn :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentSortOn.ExperimentSortOn -> ListExperimentInputBuilder ()
setSortOn value =
   Control.Monad.State.Strict.modify (\s -> (s { sort_onBuilderState = value }))

setSortBy :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy -> ListExperimentInputBuilder ()
setSortBy value =
   Control.Monad.State.Strict.modify (\s -> (s { sort_byBuilderState = value }))

setGlobalExperimentsOnly :: Data.Maybe.Maybe Bool -> ListExperimentInputBuilder ()
setGlobalExperimentsOnly value =
   Control.Monad.State.Strict.modify (\s -> (s { global_experiments_onlyBuilderState = value }))

setDimensionMatchStrategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy -> ListExperimentInputBuilder ()
setDimensionMatchStrategy value =
   Control.Monad.State.Strict.modify (\s -> (s { dimension_match_strategyBuilderState = value }))

build :: ListExperimentInputBuilder () -> Data.Either.Either Data.Text.Text ListExperimentInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentInput.ListExperimentInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentInput.ListExperimentInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    count' <- Data.Either.Right (countBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    status' <- Data.Either.Right (statusBuilderState st)
    from_date' <- Data.Either.Right (from_dateBuilderState st)
    to_date' <- Data.Either.Right (to_dateBuilderState st)
    experiment_name' <- Data.Either.Right (experiment_nameBuilderState st)
    experiment_ids' <- Data.Either.Right (experiment_idsBuilderState st)
    experiment_group_ids' <- Data.Either.Right (experiment_group_idsBuilderState st)
    created_by' <- Data.Either.Right (created_byBuilderState st)
    sort_on' <- Data.Either.Right (sort_onBuilderState st)
    sort_by' <- Data.Either.Right (sort_byBuilderState st)
    global_experiments_only' <- Data.Either.Right (global_experiments_onlyBuilderState st)
    dimension_match_strategy' <- Data.Either.Right (dimension_match_strategyBuilderState st)
    Data.Either.Right (ListExperimentInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        page = page',
        count = count',
        all' = all'',
        status = status',
        from_date = from_date',
        to_date = to_date',
        experiment_name = experiment_name',
        experiment_ids = experiment_ids',
        experiment_group_ids = experiment_group_ids',
        created_by = created_by',
        sort_on = sort_on',
        sort_by = sort_by',
        global_experiments_only = global_experiments_only',
        dimension_match_strategy = dimension_match_strategy'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListExperimentInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "experiments"
            ]
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "experiment_name" (experiment_name self)
        Io.Superposition.Utility.serQuery "from_date" (from_date self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "experiment_ids" (experiment_ids self)
        Io.Superposition.Utility.serQuery "global_experiments_only" (global_experiments_only self)
        Io.Superposition.Utility.serQuery "sort_by" (sort_by self)
        Io.Superposition.Utility.serQuery "experiment_group_ids" (experiment_group_ids self)
        Io.Superposition.Utility.serQuery "created_by" (created_by self)
        Io.Superposition.Utility.serQuery "dimension_match_strategy" (dimension_match_strategy self)
        Io.Superposition.Utility.serQuery "sort_on" (sort_on self)
        Io.Superposition.Utility.serQuery "to_date" (to_date self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serQuery "status" (status self)
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


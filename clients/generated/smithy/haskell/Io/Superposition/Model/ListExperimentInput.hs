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
    global_experiments_only
) where
import qualified Control.Applicative
import qualified Control.Monad
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
import qualified Io.Superposition.Model.ExperimentSortOn
import qualified Io.Superposition.Model.ExperimentStatusType
import qualified Io.Superposition.Model.SortBy

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
    global_experiments_only :: Data.Maybe.Maybe Bool
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
        "global_experiments_only" Data.Aeson..= global_experiments_only a
        ]
    


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
    global_experiments_onlyBuilderState :: Data.Maybe.Maybe Bool
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
    global_experiments_onlyBuilderState = Data.Maybe.Nothing
}

newtype ListExperimentInputBuilder a = ListExperimentInputBuilder {
    runListExperimentInputBuilder :: ListExperimentInputBuilderState -> (ListExperimentInputBuilderState, a)
}

instance Data.Functor.Functor ListExperimentInputBuilder where
    fmap f (ListExperimentInputBuilder g) =
        ListExperimentInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListExperimentInputBuilder where
    pure a = ListExperimentInputBuilder (\s -> (s, a))
    (ListExperimentInputBuilder f) <*> (ListExperimentInputBuilder g) = ListExperimentInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListExperimentInputBuilder where
    (ListExperimentInputBuilder f) >>= g = ListExperimentInputBuilder (\s ->
        let (s', a) = f s
            (ListExperimentInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> ListExperimentInputBuilder ()
setWorkspaceId value =
   ListExperimentInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> ListExperimentInputBuilder ()
setOrgId value =
   ListExperimentInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setPage :: Data.Maybe.Maybe Data.Int.Int64 -> ListExperimentInputBuilder ()
setPage value =
   ListExperimentInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setCount :: Data.Maybe.Maybe Data.Int.Int64 -> ListExperimentInputBuilder ()
setCount value =
   ListExperimentInputBuilder (\s -> (s { countBuilderState = value }, ()))

setAll' :: Data.Maybe.Maybe Bool -> ListExperimentInputBuilder ()
setAll' value =
   ListExperimentInputBuilder (\s -> (s { all'BuilderState = value }, ()))

setStatus :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType -> ListExperimentInputBuilder ()
setStatus value =
   ListExperimentInputBuilder (\s -> (s { statusBuilderState = value }, ()))

setFromDate :: Data.Maybe.Maybe Data.Time.UTCTime -> ListExperimentInputBuilder ()
setFromDate value =
   ListExperimentInputBuilder (\s -> (s { from_dateBuilderState = value }, ()))

setToDate :: Data.Maybe.Maybe Data.Time.UTCTime -> ListExperimentInputBuilder ()
setToDate value =
   ListExperimentInputBuilder (\s -> (s { to_dateBuilderState = value }, ()))

setExperimentName :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentInputBuilder ()
setExperimentName value =
   ListExperimentInputBuilder (\s -> (s { experiment_nameBuilderState = value }, ()))

setExperimentIds :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentInputBuilder ()
setExperimentIds value =
   ListExperimentInputBuilder (\s -> (s { experiment_idsBuilderState = value }, ()))

setExperimentGroupIds :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentInputBuilder ()
setExperimentGroupIds value =
   ListExperimentInputBuilder (\s -> (s { experiment_group_idsBuilderState = value }, ()))

setCreatedBy :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentInputBuilder ()
setCreatedBy value =
   ListExperimentInputBuilder (\s -> (s { created_byBuilderState = value }, ()))

setSortOn :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentSortOn.ExperimentSortOn -> ListExperimentInputBuilder ()
setSortOn value =
   ListExperimentInputBuilder (\s -> (s { sort_onBuilderState = value }, ()))

setSortBy :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy -> ListExperimentInputBuilder ()
setSortBy value =
   ListExperimentInputBuilder (\s -> (s { sort_byBuilderState = value }, ()))

setGlobalExperimentsOnly :: Data.Maybe.Maybe Bool -> ListExperimentInputBuilder ()
setGlobalExperimentsOnly value =
   ListExperimentInputBuilder (\s -> (s { global_experiments_onlyBuilderState = value }, ()))

build :: ListExperimentInputBuilder () -> Data.Either.Either Data.Text.Text ListExperimentInput
build builder = do
    let (st, _) = runListExperimentInputBuilder builder defaultBuilderState
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
        global_experiments_only = global_experiments_only'
    })



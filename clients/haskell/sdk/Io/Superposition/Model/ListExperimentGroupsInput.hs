module Io.Superposition.Model.ListExperimentGroupsInput (
    setCount,
    setPage,
    setAll',
    setWorkspaceId,
    setOrgId,
    setIfModifiedSince,
    setName,
    setCreatedBy,
    setLastModifiedBy,
    setSortOn,
    setSortBy,
    setGroupType,
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> c1293812 (Test)
=======
>>>>>>> de718464 (fix: more fixes)
    setDimensionMatchStrategy,
=======
>>>>>>> 6e8749e1 (Test)
=======
    setDimensionMatchStrategy,
>>>>>>> 82479b8f (fix: more fixes)
    setContext,
=======
<<<<<<< HEAD
=======
    setDimensionMatchStrategy,
    setContext,
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
    setDimensionMatchStrategy,
    setContext,
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
    build,
    ListExperimentGroupsInputBuilder,
    ListExperimentGroupsInput,
    count,
    page,
    all',
    workspace_id,
    org_id,
    if_modified_since,
    name,
    created_by,
    last_modified_by,
    sort_on,
    sort_by,
<<<<<<< HEAD
<<<<<<< HEAD
    group_type,
<<<<<<< HEAD
<<<<<<< HEAD
    dimension_match_strategy,
=======
>>>>>>> 6e8749e1 (Test)
=======
    dimension_match_strategy,
>>>>>>> 82479b8f (fix: more fixes)
    context
=======
<<<<<<< HEAD
    group_type
=======
    group_type,
    dimension_match_strategy,
    context
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
    group_type,
    dimension_match_strategy,
    context
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.DimensionMatchStrategy
import qualified Io.Superposition.Model.ExperimentGroupSortOn
import qualified Io.Superposition.Model.GroupType
import qualified Io.Superposition.Model.SortBy
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ListExperimentGroupsInput = ListExperimentGroupsInput {
    count :: Data.Maybe.Maybe Data.Int.Int32,
    page :: Data.Maybe.Maybe Data.Int.Int32,
    all' :: Data.Maybe.Maybe Bool,
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    if_modified_since :: Data.Maybe.Maybe Data.Time.UTCTime,
    name :: Data.Maybe.Maybe Data.Text.Text,
    created_by :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_by :: Data.Maybe.Maybe Data.Text.Text,
    sort_on :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentGroupSortOn.ExperimentGroupSortOn,
    sort_by :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
<<<<<<< HEAD
<<<<<<< HEAD
    group_type :: Data.Maybe.Maybe ([] Io.Superposition.Model.GroupType.GroupType),
<<<<<<< HEAD
<<<<<<< HEAD
    dimension_match_strategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy,
=======
>>>>>>> 6e8749e1 (Test)
=======
    dimension_match_strategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy,
>>>>>>> 82479b8f (fix: more fixes)
    context :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
=======
<<<<<<< HEAD
    group_type :: Data.Maybe.Maybe ([] Io.Superposition.Model.GroupType.GroupType)
=======
    group_type :: Data.Maybe.Maybe ([] Io.Superposition.Model.GroupType.GroupType),
    dimension_match_strategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy,
    context :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
    group_type :: Data.Maybe.Maybe ([] Io.Superposition.Model.GroupType.GroupType),
    dimension_match_strategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy,
    context :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListExperimentGroupsInput where
    toJSON a = Data.Aeson.object [
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "all" Data.Aeson..= all' a,
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "if_modified_since" Data.Aeson..= if_modified_since a,
        "name" Data.Aeson..= name a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "sort_on" Data.Aeson..= sort_on a,
        "sort_by" Data.Aeson..= sort_by a,
<<<<<<< HEAD
<<<<<<< HEAD
        "group_type" Data.Aeson..= group_type a,
<<<<<<< HEAD
<<<<<<< HEAD
        "dimension_match_strategy" Data.Aeson..= dimension_match_strategy a,
=======
>>>>>>> 6e8749e1 (Test)
=======
        "dimension_match_strategy" Data.Aeson..= dimension_match_strategy a,
>>>>>>> 82479b8f (fix: more fixes)
        "context" Data.Aeson..= context a
=======
<<<<<<< HEAD
        "group_type" Data.Aeson..= group_type a
=======
        "group_type" Data.Aeson..= group_type a,
        "dimension_match_strategy" Data.Aeson..= dimension_match_strategy a,
        "context" Data.Aeson..= context a
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
        "group_type" Data.Aeson..= group_type a,
        "dimension_match_strategy" Data.Aeson..= dimension_match_strategy a,
        "context" Data.Aeson..= context a
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
        ]
    

instance Io.Superposition.Utility.SerializeBody ListExperimentGroupsInput

instance Data.Aeson.FromJSON ListExperimentGroupsInput where
    parseJSON = Data.Aeson.withObject "ListExperimentGroupsInput" $ \v -> ListExperimentGroupsInput
        Data.Functor.<$> (v Data.Aeson..:? "count")
        Control.Applicative.<*> (v Data.Aeson..:? "page")
        Control.Applicative.<*> (v Data.Aeson..:? "all")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "if_modified_since")
        Control.Applicative.<*> (v Data.Aeson..:? "name")
        Control.Applicative.<*> (v Data.Aeson..:? "created_by")
        Control.Applicative.<*> (v Data.Aeson..:? "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..:? "sort_on")
        Control.Applicative.<*> (v Data.Aeson..:? "sort_by")
        Control.Applicative.<*> (v Data.Aeson..:? "group_type")
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> c1293812 (Test)
=======
>>>>>>> de718464 (fix: more fixes)
        Control.Applicative.<*> (v Data.Aeson..:? "dimension_match_strategy")
=======
>>>>>>> 6e8749e1 (Test)
=======
        Control.Applicative.<*> (v Data.Aeson..:? "dimension_match_strategy")
>>>>>>> 82479b8f (fix: more fixes)
        Control.Applicative.<*> (v Data.Aeson..:? "context")
=======
<<<<<<< HEAD
=======
        Control.Applicative.<*> (v Data.Aeson..:? "dimension_match_strategy")
        Control.Applicative.<*> (v Data.Aeson..:? "context")
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
        Control.Applicative.<*> (v Data.Aeson..:? "dimension_match_strategy")
        Control.Applicative.<*> (v Data.Aeson..:? "context")
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
    



data ListExperimentGroupsInputBuilderState = ListExperimentGroupsInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    if_modified_sinceBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    sort_onBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentGroupSortOn.ExperimentGroupSortOn,
    sort_byBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
<<<<<<< HEAD
<<<<<<< HEAD
    group_typeBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.GroupType.GroupType),
<<<<<<< HEAD
<<<<<<< HEAD
    dimension_match_strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy,
=======
>>>>>>> 6e8749e1 (Test)
=======
    dimension_match_strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy,
>>>>>>> 82479b8f (fix: more fixes)
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
=======
<<<<<<< HEAD
    group_typeBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.GroupType.GroupType)
=======
    group_typeBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.GroupType.GroupType),
    dimension_match_strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
    group_typeBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.GroupType.GroupType),
    dimension_match_strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListExperimentGroupsInputBuilderState
defaultBuilderState = ListExperimentGroupsInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    if_modified_sinceBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    sort_onBuilderState = Data.Maybe.Nothing,
    sort_byBuilderState = Data.Maybe.Nothing,
<<<<<<< HEAD
<<<<<<< HEAD
    group_typeBuilderState = Data.Maybe.Nothing,
<<<<<<< HEAD
<<<<<<< HEAD
    dimension_match_strategyBuilderState = Data.Maybe.Nothing,
=======
>>>>>>> 6e8749e1 (Test)
=======
    dimension_match_strategyBuilderState = Data.Maybe.Nothing,
>>>>>>> 82479b8f (fix: more fixes)
    contextBuilderState = Data.Maybe.Nothing
=======
<<<<<<< HEAD
    group_typeBuilderState = Data.Maybe.Nothing
=======
    group_typeBuilderState = Data.Maybe.Nothing,
    dimension_match_strategyBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
    group_typeBuilderState = Data.Maybe.Nothing,
    dimension_match_strategyBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
}

type ListExperimentGroupsInputBuilder = Control.Monad.State.Strict.State ListExperimentGroupsInputBuilderState

setCount :: Data.Maybe.Maybe Data.Int.Int32 -> ListExperimentGroupsInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> ListExperimentGroupsInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setAll' :: Data.Maybe.Maybe Bool -> ListExperimentGroupsInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setWorkspaceId :: Data.Text.Text -> ListExperimentGroupsInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListExperimentGroupsInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setIfModifiedSince :: Data.Maybe.Maybe Data.Time.UTCTime -> ListExperimentGroupsInputBuilder ()
setIfModifiedSince value =
   Control.Monad.State.Strict.modify (\s -> (s { if_modified_sinceBuilderState = value }))

setName :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentGroupsInputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = value }))

setCreatedBy :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentGroupsInputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = value }))

setLastModifiedBy :: Data.Maybe.Maybe Data.Text.Text -> ListExperimentGroupsInputBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = value }))

setSortOn :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentGroupSortOn.ExperimentGroupSortOn -> ListExperimentGroupsInputBuilder ()
setSortOn value =
   Control.Monad.State.Strict.modify (\s -> (s { sort_onBuilderState = value }))

setSortBy :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy -> ListExperimentGroupsInputBuilder ()
setSortBy value =
   Control.Monad.State.Strict.modify (\s -> (s { sort_byBuilderState = value }))

setGroupType :: Data.Maybe.Maybe ([] Io.Superposition.Model.GroupType.GroupType) -> ListExperimentGroupsInputBuilder ()
setGroupType value =
   Control.Monad.State.Strict.modify (\s -> (s { group_typeBuilderState = value }))

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
>>>>>>> 91d47048 (fix: more fixes)
=======
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
>>>>>>> c1293812 (Test)
=======
=======
>>>>>>> 82479b8f (fix: more fixes)
>>>>>>> de718464 (fix: more fixes)
setDimensionMatchStrategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy -> ListExperimentGroupsInputBuilder ()
setDimensionMatchStrategy value =
   Control.Monad.State.Strict.modify (\s -> (s { dimension_match_strategyBuilderState = value }))

<<<<<<< HEAD
=======
>>>>>>> 6e8749e1 (Test)
=======
>>>>>>> 82479b8f (fix: more fixes)
setContext :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> ListExperimentGroupsInputBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = value }))

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
build :: ListExperimentGroupsInputBuilder () -> Data.Either.Either Data.Text.Text ListExperimentGroupsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentGroupsInput.ListExperimentGroupsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentGroupsInput.ListExperimentGroupsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    if_modified_since' <- Data.Either.Right (if_modified_sinceBuilderState st)
    name' <- Data.Either.Right (nameBuilderState st)
    created_by' <- Data.Either.Right (created_byBuilderState st)
    last_modified_by' <- Data.Either.Right (last_modified_byBuilderState st)
    sort_on' <- Data.Either.Right (sort_onBuilderState st)
    sort_by' <- Data.Either.Right (sort_byBuilderState st)
    group_type' <- Data.Either.Right (group_typeBuilderState st)
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> c1293812 (Test)
=======
>>>>>>> de718464 (fix: more fixes)
    dimension_match_strategy' <- Data.Either.Right (dimension_match_strategyBuilderState st)
=======
>>>>>>> 6e8749e1 (Test)
=======
    dimension_match_strategy' <- Data.Either.Right (dimension_match_strategyBuilderState st)
>>>>>>> 82479b8f (fix: more fixes)
    context' <- Data.Either.Right (contextBuilderState st)
=======
<<<<<<< HEAD
=======
    dimension_match_strategy' <- Data.Either.Right (dimension_match_strategyBuilderState st)
    context' <- Data.Either.Right (contextBuilderState st)
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
    dimension_match_strategy' <- Data.Either.Right (dimension_match_strategyBuilderState st)
    context' <- Data.Either.Right (contextBuilderState st)
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
    Data.Either.Right (ListExperimentGroupsInput { 
        count = count',
        page = page',
        all' = all'',
        workspace_id = workspace_id',
        org_id = org_id',
        if_modified_since = if_modified_since',
        name = name',
        created_by = created_by',
        last_modified_by = last_modified_by',
        sort_on = sort_on',
        sort_by = sort_by',
<<<<<<< HEAD
<<<<<<< HEAD
        group_type = group_type',
<<<<<<< HEAD
<<<<<<< HEAD
        dimension_match_strategy = dimension_match_strategy',
=======
>>>>>>> 6e8749e1 (Test)
=======
        dimension_match_strategy = dimension_match_strategy',
>>>>>>> 82479b8f (fix: more fixes)
        context = context'
=======
<<<<<<< HEAD
        group_type = group_type'
=======
        group_type = group_type',
        dimension_match_strategy = dimension_match_strategy',
        context = context'
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
        group_type = group_type',
        dimension_match_strategy = dimension_match_strategy',
        context = context'
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListExperimentGroupsInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "experiment-groups",
            "list"
            ]
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "last_modified_by" (last_modified_by self)
        Io.Superposition.Utility.serQuery "sort_by" (sort_by self)
        Io.Superposition.Utility.serQuery "group_type" (group_type self)
        Io.Superposition.Utility.serQuery "created_by" (created_by self)
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
>>>>>>> 91d47048 (fix: more fixes)
=======
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
>>>>>>> c1293812 (Test)
=======
>>>>>>> de718464 (fix: more fixes)
        Io.Superposition.Utility.serQuery "dimension_match_strategy" (dimension_match_strategy self)
        Io.Superposition.Utility.serQuery "sort_on" (sort_on self)
        Io.Superposition.Utility.serQuery "name" (name self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serHeader "if-modified-since" (if_modified_since self)
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
=======
        Io.Superposition.Utility.serQuery "sort_on" (sort_on self)
        Io.Superposition.Utility.serQuery "name" (name self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serHeader "If-Modified-Since" (if_modified_since self)
>>>>>>> 6e8749e1 (Test)
<<<<<<< HEAD
>>>>>>> c1293812 (Test)
=======
=======
        Io.Superposition.Utility.serQuery "dimension_match_strategy" (dimension_match_strategy self)
        Io.Superposition.Utility.serQuery "sort_on" (sort_on self)
        Io.Superposition.Utility.serQuery "name" (name self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serHeader "if-modified-since" (if_modified_since self)
>>>>>>> 82479b8f (fix: more fixes)
>>>>>>> de718464 (fix: more fixes)
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "context" (context self)


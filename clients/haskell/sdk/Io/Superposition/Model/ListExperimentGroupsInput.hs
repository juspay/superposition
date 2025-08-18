module Io.Superposition.Model.ListExperimentGroupsInput (
    setWorkspaceId,
    setOrgId,
    setPage,
    setCount,
    setName,
    setCreatedBy,
    setLastModifiedBy,
    setSortOn,
    setSortBy,
    setAll',
    setGroupType,
    build,
    ListExperimentGroupsInputBuilder,
    ListExperimentGroupsInput,
    workspace_id,
    org_id,
    page,
    count,
    name,
    created_by,
    last_modified_by,
    sort_on,
    sort_by,
    all',
    group_type
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ExperimentGroupSortOn
import qualified Io.Superposition.Model.GroupType
import qualified Io.Superposition.Model.SortBy
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ListExperimentGroupsInput = ListExperimentGroupsInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    page :: Data.Maybe.Maybe Data.Int.Int64,
    count :: Data.Maybe.Maybe Data.Int.Int64,
    name :: Data.Maybe.Maybe Data.Text.Text,
    created_by :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_by :: Data.Maybe.Maybe Data.Text.Text,
    sort_on :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentGroupSortOn.ExperimentGroupSortOn,
    sort_by :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
    all' :: Data.Maybe.Maybe Bool,
    group_type :: Data.Maybe.Maybe Io.Superposition.Model.GroupType.GroupType
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListExperimentGroupsInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "page" Data.Aeson..= page a,
        "count" Data.Aeson..= count a,
        "name" Data.Aeson..= name a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "sort_on" Data.Aeson..= sort_on a,
        "sort_by" Data.Aeson..= sort_by a,
        "all" Data.Aeson..= all' a,
        "group_type" Data.Aeson..= group_type a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListExperimentGroupsInput

instance Data.Aeson.FromJSON ListExperimentGroupsInput where
    parseJSON = Data.Aeson.withObject "ListExperimentGroupsInput" $ \v -> ListExperimentGroupsInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "sort_on")
        Control.Applicative.<*> (v Data.Aeson..: "sort_by")
        Control.Applicative.<*> (v Data.Aeson..: "all")
        Control.Applicative.<*> (v Data.Aeson..: "group_type")
    



data ListExperimentGroupsInputBuilderState = ListExperimentGroupsInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    sort_onBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentGroupSortOn.ExperimentGroupSortOn,
    sort_byBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    group_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.GroupType.GroupType
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListExperimentGroupsInputBuilderState
defaultBuilderState = ListExperimentGroupsInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    countBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    sort_onBuilderState = Data.Maybe.Nothing,
    sort_byBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    group_typeBuilderState = Data.Maybe.Nothing
}

type ListExperimentGroupsInputBuilder = Control.Monad.State.Strict.State ListExperimentGroupsInputBuilderState

setWorkspaceId :: Data.Text.Text -> ListExperimentGroupsInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListExperimentGroupsInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setPage :: Data.Maybe.Maybe Data.Int.Int64 -> ListExperimentGroupsInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setCount :: Data.Maybe.Maybe Data.Int.Int64 -> ListExperimentGroupsInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

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

setAll' :: Data.Maybe.Maybe Bool -> ListExperimentGroupsInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setGroupType :: Data.Maybe.Maybe Io.Superposition.Model.GroupType.GroupType -> ListExperimentGroupsInputBuilder ()
setGroupType value =
   Control.Monad.State.Strict.modify (\s -> (s { group_typeBuilderState = value }))

build :: ListExperimentGroupsInputBuilder () -> Data.Either.Either Data.Text.Text ListExperimentGroupsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentGroupsInput.ListExperimentGroupsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentGroupsInput.ListExperimentGroupsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    count' <- Data.Either.Right (countBuilderState st)
    name' <- Data.Either.Right (nameBuilderState st)
    created_by' <- Data.Either.Right (created_byBuilderState st)
    last_modified_by' <- Data.Either.Right (last_modified_byBuilderState st)
    sort_on' <- Data.Either.Right (sort_onBuilderState st)
    sort_by' <- Data.Either.Right (sort_byBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    group_type' <- Data.Either.Right (group_typeBuilderState st)
    Data.Either.Right (ListExperimentGroupsInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        page = page',
        count = count',
        name = name',
        created_by = created_by',
        last_modified_by = last_modified_by',
        sort_on = sort_on',
        sort_by = sort_by',
        all' = all'',
        group_type = group_type'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListExperimentGroupsInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "experiment-groups"
            ]
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "sort_on" (sort_on self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "name" (name self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serQuery "last_modified_by" (last_modified_by self)
        Io.Superposition.Utility.serQuery "sort_by" (sort_by self)
        Io.Superposition.Utility.serQuery "group_type" (group_type self)
        Io.Superposition.Utility.serQuery "created_by" (created_by self)
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


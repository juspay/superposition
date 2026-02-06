module Io.Superposition.Model.ListVariablesInput (
    setCount,
    setPage,
    setAll',
    setWorkspaceId,
    setOrgId,
    setName,
    setCreatedBy,
    setLastModifiedBy,
    setSortOn,
    setSortBy,
    build,
    ListVariablesInputBuilder,
    ListVariablesInput,
    count,
    page,
    all',
    workspace_id,
    org_id,
    name,
    created_by,
    last_modified_by,
    sort_on,
    sort_by
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
import qualified Io.Superposition.Model.SortBy
import qualified Io.Superposition.Model.VariableSortOn
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ListVariablesInput = ListVariablesInput {
    count :: Data.Maybe.Maybe Data.Int.Int32,
    page :: Data.Maybe.Maybe Data.Int.Int32,
    all' :: Data.Maybe.Maybe Bool,
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    name :: Data.Maybe.Maybe ([] Data.Text.Text),
    created_by :: Data.Maybe.Maybe ([] Data.Text.Text),
    last_modified_by :: Data.Maybe.Maybe ([] Data.Text.Text),
    sort_on :: Data.Maybe.Maybe Io.Superposition.Model.VariableSortOn.VariableSortOn,
    sort_by :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListVariablesInput where
    toJSON a = Data.Aeson.object [
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "all" Data.Aeson..= all' a,
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "name" Data.Aeson..= name a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "sort_on" Data.Aeson..= sort_on a,
        "sort_by" Data.Aeson..= sort_by a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListVariablesInput

instance Data.Aeson.FromJSON ListVariablesInput where
    parseJSON = Data.Aeson.withObject "ListVariablesInput" $ \v -> ListVariablesInput
        Data.Functor.<$> (v Data.Aeson..:? "count")
        Control.Applicative.<*> (v Data.Aeson..:? "page")
        Control.Applicative.<*> (v Data.Aeson..:? "all")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "name")
        Control.Applicative.<*> (v Data.Aeson..:? "created_by")
        Control.Applicative.<*> (v Data.Aeson..:? "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..:? "sort_on")
        Control.Applicative.<*> (v Data.Aeson..:? "sort_by")
    



data ListVariablesInputBuilderState = ListVariablesInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    created_byBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    last_modified_byBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    sort_onBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.VariableSortOn.VariableSortOn,
    sort_byBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListVariablesInputBuilderState
defaultBuilderState = ListVariablesInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    sort_onBuilderState = Data.Maybe.Nothing,
    sort_byBuilderState = Data.Maybe.Nothing
}

type ListVariablesInputBuilder = Control.Monad.State.Strict.State ListVariablesInputBuilderState

setCount :: Data.Maybe.Maybe Data.Int.Int32 -> ListVariablesInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> ListVariablesInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setAll' :: Data.Maybe.Maybe Bool -> ListVariablesInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setWorkspaceId :: Data.Text.Text -> ListVariablesInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListVariablesInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setName :: Data.Maybe.Maybe ([] Data.Text.Text) -> ListVariablesInputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = value }))

setCreatedBy :: Data.Maybe.Maybe ([] Data.Text.Text) -> ListVariablesInputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = value }))

setLastModifiedBy :: Data.Maybe.Maybe ([] Data.Text.Text) -> ListVariablesInputBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = value }))

setSortOn :: Data.Maybe.Maybe Io.Superposition.Model.VariableSortOn.VariableSortOn -> ListVariablesInputBuilder ()
setSortOn value =
   Control.Monad.State.Strict.modify (\s -> (s { sort_onBuilderState = value }))

setSortBy :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy -> ListVariablesInputBuilder ()
setSortBy value =
   Control.Monad.State.Strict.modify (\s -> (s { sort_byBuilderState = value }))

build :: ListVariablesInputBuilder () -> Data.Either.Either Data.Text.Text ListVariablesInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVariablesInput.ListVariablesInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVariablesInput.ListVariablesInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    name' <- Data.Either.Right (nameBuilderState st)
    created_by' <- Data.Either.Right (created_byBuilderState st)
    last_modified_by' <- Data.Either.Right (last_modified_byBuilderState st)
    sort_on' <- Data.Either.Right (sort_onBuilderState st)
    sort_by' <- Data.Either.Right (sort_byBuilderState st)
    Data.Either.Right (ListVariablesInput { 
        count = count',
        page = page',
        all' = all'',
        workspace_id = workspace_id',
        org_id = org_id',
        name = name',
        created_by = created_by',
        last_modified_by = last_modified_by',
        sort_on = sort_on',
        sort_by = sort_by'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListVariablesInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "variables"
            ]
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "sort_on" (sort_on self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "name" (name self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serQuery "last_modified_by" (last_modified_by self)
        Io.Superposition.Utility.serQuery "sort_by" (sort_by self)
        Io.Superposition.Utility.serQuery "created_by" (created_by self)
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


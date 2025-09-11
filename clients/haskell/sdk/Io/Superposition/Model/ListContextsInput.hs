module Io.Superposition.Model.ListContextsInput (
    setWorkspaceId,
    setOrgId,
    setPage,
    setCount,
    setAll',
    setPrefix,
    setSortOn,
    setSortBy,
    setCreatedBy,
    setLastModifiedBy,
    setPlaintext,
    setDimensionMatchStrategy,
    build,
    ListContextsInputBuilder,
    ListContextsInput,
    workspace_id,
    org_id,
    page,
    count,
    all',
    prefix,
    sort_on,
    sort_by,
    created_by,
    last_modified_by,
    plaintext,
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ContextFilterSortOn
import qualified Io.Superposition.Model.DimensionMatchStrategy
import qualified Io.Superposition.Model.SortBy
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ListContextsInput = ListContextsInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    page :: Data.Maybe.Maybe Data.Int.Int32,
    count :: Data.Maybe.Maybe Data.Int.Int32,
    all' :: Data.Maybe.Maybe Bool,
    prefix :: Data.Maybe.Maybe Data.Text.Text,
    sort_on :: Data.Maybe.Maybe Io.Superposition.Model.ContextFilterSortOn.ContextFilterSortOn,
    sort_by :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
    created_by :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_by :: Data.Maybe.Maybe Data.Text.Text,
    plaintext :: Data.Maybe.Maybe Data.Text.Text,
    dimension_match_strategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListContextsInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "page" Data.Aeson..= page a,
        "count" Data.Aeson..= count a,
        "all" Data.Aeson..= all' a,
        "prefix" Data.Aeson..= prefix a,
        "sort_on" Data.Aeson..= sort_on a,
        "sort_by" Data.Aeson..= sort_by a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "plaintext" Data.Aeson..= plaintext a,
        "dimension_match_strategy" Data.Aeson..= dimension_match_strategy a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListContextsInput

instance Data.Aeson.FromJSON ListContextsInput where
    parseJSON = Data.Aeson.withObject "ListContextsInput" $ \v -> ListContextsInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "all")
        Control.Applicative.<*> (v Data.Aeson..: "prefix")
        Control.Applicative.<*> (v Data.Aeson..: "sort_on")
        Control.Applicative.<*> (v Data.Aeson..: "sort_by")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "plaintext")
        Control.Applicative.<*> (v Data.Aeson..: "dimension_match_strategy")
    



data ListContextsInputBuilderState = ListContextsInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    prefixBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    sort_onBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ContextFilterSortOn.ContextFilterSortOn,
    sort_byBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    plaintextBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimension_match_strategyBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListContextsInputBuilderState
defaultBuilderState = ListContextsInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    countBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    prefixBuilderState = Data.Maybe.Nothing,
    sort_onBuilderState = Data.Maybe.Nothing,
    sort_byBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    plaintextBuilderState = Data.Maybe.Nothing,
    dimension_match_strategyBuilderState = Data.Maybe.Nothing
}

type ListContextsInputBuilder = Control.Monad.State.Strict.State ListContextsInputBuilderState

setWorkspaceId :: Data.Text.Text -> ListContextsInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListContextsInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> ListContextsInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setCount :: Data.Maybe.Maybe Data.Int.Int32 -> ListContextsInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setAll' :: Data.Maybe.Maybe Bool -> ListContextsInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setPrefix :: Data.Maybe.Maybe Data.Text.Text -> ListContextsInputBuilder ()
setPrefix value =
   Control.Monad.State.Strict.modify (\s -> (s { prefixBuilderState = value }))

setSortOn :: Data.Maybe.Maybe Io.Superposition.Model.ContextFilterSortOn.ContextFilterSortOn -> ListContextsInputBuilder ()
setSortOn value =
   Control.Monad.State.Strict.modify (\s -> (s { sort_onBuilderState = value }))

setSortBy :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy -> ListContextsInputBuilder ()
setSortBy value =
   Control.Monad.State.Strict.modify (\s -> (s { sort_byBuilderState = value }))

setCreatedBy :: Data.Maybe.Maybe Data.Text.Text -> ListContextsInputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = value }))

setLastModifiedBy :: Data.Maybe.Maybe Data.Text.Text -> ListContextsInputBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = value }))

setPlaintext :: Data.Maybe.Maybe Data.Text.Text -> ListContextsInputBuilder ()
setPlaintext value =
   Control.Monad.State.Strict.modify (\s -> (s { plaintextBuilderState = value }))

setDimensionMatchStrategy :: Data.Maybe.Maybe Io.Superposition.Model.DimensionMatchStrategy.DimensionMatchStrategy -> ListContextsInputBuilder ()
setDimensionMatchStrategy value =
   Control.Monad.State.Strict.modify (\s -> (s { dimension_match_strategyBuilderState = value }))

build :: ListContextsInputBuilder () -> Data.Either.Either Data.Text.Text ListContextsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListContextsInput.ListContextsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListContextsInput.ListContextsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    count' <- Data.Either.Right (countBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    prefix' <- Data.Either.Right (prefixBuilderState st)
    sort_on' <- Data.Either.Right (sort_onBuilderState st)
    sort_by' <- Data.Either.Right (sort_byBuilderState st)
    created_by' <- Data.Either.Right (created_byBuilderState st)
    last_modified_by' <- Data.Either.Right (last_modified_byBuilderState st)
    plaintext' <- Data.Either.Right (plaintextBuilderState st)
    dimension_match_strategy' <- Data.Either.Right (dimension_match_strategyBuilderState st)
    Data.Either.Right (ListContextsInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        page = page',
        count = count',
        all' = all'',
        prefix = prefix',
        sort_on = sort_on',
        sort_by = sort_by',
        created_by = created_by',
        last_modified_by = last_modified_by',
        plaintext = plaintext',
        dimension_match_strategy = dimension_match_strategy'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListContextsInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "context",
            "list"
            ]
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "dimension_match_strategy" (dimension_match_strategy self)
        Io.Superposition.Utility.serQuery "sort_on" (sort_on self)
        Io.Superposition.Utility.serQuery "prefix" (prefix self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "plaintext" (plaintext self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serQuery "sort_by" (sort_by self)
        Io.Superposition.Utility.serQuery "last_modified_by" (last_modified_by self)
        Io.Superposition.Utility.serQuery "created_by" (created_by self)
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


module Io.Superposition.Model.ListGroupedDefaultConfigsInput (
    setWorkspaceId,
    setOrgId,
    setCount,
    setPage,
    setAll',
    setName,
    setPrefix,
    setSortBy,
    setSortOn,
    build,
    ListGroupedDefaultConfigsInputBuilder,
    ListGroupedDefaultConfigsInput,
    workspace_id,
    org_id,
    count,
    page,
    all',
    name,
    prefix,
    sort_by,
    sort_on
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
import qualified Io.Superposition.Model.DefaultConfigSortOn
import qualified Io.Superposition.Model.SortBy
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ListGroupedDefaultConfigsInput = ListGroupedDefaultConfigsInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    count :: Data.Maybe.Maybe Data.Int.Int32,
    page :: Data.Maybe.Maybe Data.Int.Int32,
    all' :: Data.Maybe.Maybe Bool,
    name :: Data.Maybe.Maybe ([] Data.Text.Text),
    prefix :: Data.Maybe.Maybe Data.Text.Text,
    sort_by :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
    sort_on :: Data.Maybe.Maybe Io.Superposition.Model.DefaultConfigSortOn.DefaultConfigSortOn
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListGroupedDefaultConfigsInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "all" Data.Aeson..= all' a,
        "name" Data.Aeson..= name a,
        "prefix" Data.Aeson..= prefix a,
        "sort_by" Data.Aeson..= sort_by a,
        "sort_on" Data.Aeson..= sort_on a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListGroupedDefaultConfigsInput

instance Data.Aeson.FromJSON ListGroupedDefaultConfigsInput where
    parseJSON = Data.Aeson.withObject "ListGroupedDefaultConfigsInput" $ \v -> ListGroupedDefaultConfigsInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "count")
        Control.Applicative.<*> (v Data.Aeson..:? "page")
        Control.Applicative.<*> (v Data.Aeson..:? "all")
        Control.Applicative.<*> (v Data.Aeson..:? "name")
        Control.Applicative.<*> (v Data.Aeson..:? "prefix")
        Control.Applicative.<*> (v Data.Aeson..:? "sort_by")
        Control.Applicative.<*> (v Data.Aeson..:? "sort_on")
    



data ListGroupedDefaultConfigsInputBuilderState = ListGroupedDefaultConfigsInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    nameBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    prefixBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    sort_byBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
    sort_onBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DefaultConfigSortOn.DefaultConfigSortOn
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListGroupedDefaultConfigsInputBuilderState
defaultBuilderState = ListGroupedDefaultConfigsInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    prefixBuilderState = Data.Maybe.Nothing,
    sort_byBuilderState = Data.Maybe.Nothing,
    sort_onBuilderState = Data.Maybe.Nothing
}

type ListGroupedDefaultConfigsInputBuilder = Control.Monad.State.Strict.State ListGroupedDefaultConfigsInputBuilderState

setWorkspaceId :: Data.Text.Text -> ListGroupedDefaultConfigsInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListGroupedDefaultConfigsInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setCount :: Data.Maybe.Maybe Data.Int.Int32 -> ListGroupedDefaultConfigsInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> ListGroupedDefaultConfigsInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setAll' :: Data.Maybe.Maybe Bool -> ListGroupedDefaultConfigsInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setName :: Data.Maybe.Maybe ([] Data.Text.Text) -> ListGroupedDefaultConfigsInputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = value }))

setPrefix :: Data.Maybe.Maybe Data.Text.Text -> ListGroupedDefaultConfigsInputBuilder ()
setPrefix value =
   Control.Monad.State.Strict.modify (\s -> (s { prefixBuilderState = value }))

setSortBy :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy -> ListGroupedDefaultConfigsInputBuilder ()
setSortBy value =
   Control.Monad.State.Strict.modify (\s -> (s { sort_byBuilderState = value }))

setSortOn :: Data.Maybe.Maybe Io.Superposition.Model.DefaultConfigSortOn.DefaultConfigSortOn -> ListGroupedDefaultConfigsInputBuilder ()
setSortOn value =
   Control.Monad.State.Strict.modify (\s -> (s { sort_onBuilderState = value }))

build :: ListGroupedDefaultConfigsInputBuilder () -> Data.Either.Either Data.Text.Text ListGroupedDefaultConfigsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListGroupedDefaultConfigsInput.ListGroupedDefaultConfigsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListGroupedDefaultConfigsInput.ListGroupedDefaultConfigsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    name' <- Data.Either.Right (nameBuilderState st)
    prefix' <- Data.Either.Right (prefixBuilderState st)
    sort_by' <- Data.Either.Right (sort_byBuilderState st)
    sort_on' <- Data.Either.Right (sort_onBuilderState st)
    Data.Either.Right (ListGroupedDefaultConfigsInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        count = count',
        page = page',
        all' = all'',
        name = name',
        prefix = prefix',
        sort_by = sort_by',
        sort_on = sort_on'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListGroupedDefaultConfigsInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "default-config"
            ]
        Io.Superposition.Utility.serQuery "grouped" ("true" :: Data.Text.Text)
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "sort_on" (sort_on self)
        Io.Superposition.Utility.serQuery "prefix" (prefix self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "name" (name self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serQuery "sort_by" (sort_by self)
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


module Io.Superposition.Model.ListContextsInput (
    setWorkspaceId,
    setOrgId,
    setPage,
    setCount,
    setPrefix,
    setSortOn,
    setSortBy,
    setCreatedBy,
    setLastModifiedBy,
    setPlaintext,
    build,
    ListContextsInputBuilder,
    ListContextsInput,
    workspace_id,
    org_id,
    page,
    count,
    prefix,
    sort_on,
    sort_by,
    created_by,
    last_modified_by,
    plaintext
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ContextFilterSortOn
import qualified Io.Superposition.Model.SortBy

data ListContextsInput = ListContextsInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    page :: Data.Maybe.Maybe Integer,
    count :: Data.Maybe.Maybe Integer,
    prefix :: Data.Maybe.Maybe Data.Text.Text,
    sort_on :: Data.Maybe.Maybe Io.Superposition.Model.ContextFilterSortOn.ContextFilterSortOn,
    sort_by :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
    created_by :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_by :: Data.Maybe.Maybe Data.Text.Text,
    plaintext :: Data.Maybe.Maybe Data.Text.Text
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
        "prefix" Data.Aeson..= prefix a,
        "sort_on" Data.Aeson..= sort_on a,
        "sort_by" Data.Aeson..= sort_by a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "plaintext" Data.Aeson..= plaintext a
        ]
    


instance Data.Aeson.FromJSON ListContextsInput where
    parseJSON = Data.Aeson.withObject "ListContextsInput" $ \v -> ListContextsInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "prefix")
        Control.Applicative.<*> (v Data.Aeson..: "sort_on")
        Control.Applicative.<*> (v Data.Aeson..: "sort_by")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "plaintext")
    



data ListContextsInputBuilderState = ListContextsInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    pageBuilderState :: Data.Maybe.Maybe Integer,
    countBuilderState :: Data.Maybe.Maybe Integer,
    prefixBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    sort_onBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ContextFilterSortOn.ContextFilterSortOn,
    sort_byBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    plaintextBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListContextsInputBuilderState
defaultBuilderState = ListContextsInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    countBuilderState = Data.Maybe.Nothing,
    prefixBuilderState = Data.Maybe.Nothing,
    sort_onBuilderState = Data.Maybe.Nothing,
    sort_byBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    plaintextBuilderState = Data.Maybe.Nothing
}

newtype ListContextsInputBuilder a = ListContextsInputBuilder {
    runListContextsInputBuilder :: ListContextsInputBuilderState -> (ListContextsInputBuilderState, a)
}

instance Data.Functor.Functor ListContextsInputBuilder where
    fmap f (ListContextsInputBuilder g) =
        ListContextsInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListContextsInputBuilder where
    pure a = ListContextsInputBuilder (\s -> (s, a))
    (ListContextsInputBuilder f) <*> (ListContextsInputBuilder g) = ListContextsInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListContextsInputBuilder where
    (ListContextsInputBuilder f) >>= g = ListContextsInputBuilder (\s ->
        let (s', a) = f s
            (ListContextsInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> ListContextsInputBuilder ()
setWorkspaceId value =
   ListContextsInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> ListContextsInputBuilder ()
setOrgId value =
   ListContextsInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setPage :: Data.Maybe.Maybe Integer -> ListContextsInputBuilder ()
setPage value =
   ListContextsInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setCount :: Data.Maybe.Maybe Integer -> ListContextsInputBuilder ()
setCount value =
   ListContextsInputBuilder (\s -> (s { countBuilderState = value }, ()))

setPrefix :: Data.Maybe.Maybe Data.Text.Text -> ListContextsInputBuilder ()
setPrefix value =
   ListContextsInputBuilder (\s -> (s { prefixBuilderState = value }, ()))

setSortOn :: Data.Maybe.Maybe Io.Superposition.Model.ContextFilterSortOn.ContextFilterSortOn -> ListContextsInputBuilder ()
setSortOn value =
   ListContextsInputBuilder (\s -> (s { sort_onBuilderState = value }, ()))

setSortBy :: Data.Maybe.Maybe Io.Superposition.Model.SortBy.SortBy -> ListContextsInputBuilder ()
setSortBy value =
   ListContextsInputBuilder (\s -> (s { sort_byBuilderState = value }, ()))

setCreatedBy :: Data.Maybe.Maybe Data.Text.Text -> ListContextsInputBuilder ()
setCreatedBy value =
   ListContextsInputBuilder (\s -> (s { created_byBuilderState = value }, ()))

setLastModifiedBy :: Data.Maybe.Maybe Data.Text.Text -> ListContextsInputBuilder ()
setLastModifiedBy value =
   ListContextsInputBuilder (\s -> (s { last_modified_byBuilderState = value }, ()))

setPlaintext :: Data.Maybe.Maybe Data.Text.Text -> ListContextsInputBuilder ()
setPlaintext value =
   ListContextsInputBuilder (\s -> (s { plaintextBuilderState = value }, ()))

build :: ListContextsInputBuilder () -> Data.Either.Either Data.Text.Text ListContextsInput
build builder = do
    let (st, _) = runListContextsInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListContextsInput.ListContextsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListContextsInput.ListContextsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    count' <- Data.Either.Right (countBuilderState st)
    prefix' <- Data.Either.Right (prefixBuilderState st)
    sort_on' <- Data.Either.Right (sort_onBuilderState st)
    sort_by' <- Data.Either.Right (sort_byBuilderState st)
    created_by' <- Data.Either.Right (created_byBuilderState st)
    last_modified_by' <- Data.Either.Right (last_modified_byBuilderState st)
    plaintext' <- Data.Either.Right (plaintextBuilderState st)
    Data.Either.Right (ListContextsInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        page = page',
        count = count',
        prefix = prefix',
        sort_on = sort_on',
        sort_by = sort_by',
        created_by = created_by',
        last_modified_by = last_modified_by',
        plaintext = plaintext'
    })



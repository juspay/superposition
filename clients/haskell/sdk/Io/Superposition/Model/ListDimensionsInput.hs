module Io.Superposition.Model.ListDimensionsInput (
    setCount,
    setPage,
    setAll',
    setWorkspaceId,
    setOrgId,
    build,
    ListDimensionsInputBuilder,
    ListDimensionsInput,
    count,
    page,
    all',
    workspace_id,
    org_id
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

data ListDimensionsInput = ListDimensionsInput {
    count :: Data.Maybe.Maybe Integer,
    page :: Data.Maybe.Maybe Integer,
    all' :: Data.Maybe.Maybe Bool,
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListDimensionsInput where
    toJSON a = Data.Aeson.object [
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "all" Data.Aeson..= all' a,
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a
        ]
    


instance Data.Aeson.FromJSON ListDimensionsInput where
    parseJSON = Data.Aeson.withObject "ListDimensionsInput" $ \v -> ListDimensionsInput
        Data.Functor.<$> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "all")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
    



data ListDimensionsInputBuilderState = ListDimensionsInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Integer,
    pageBuilderState :: Data.Maybe.Maybe Integer,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListDimensionsInputBuilderState
defaultBuilderState = ListDimensionsInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing
}

newtype ListDimensionsInputBuilder a = ListDimensionsInputBuilder {
    runListDimensionsInputBuilder :: ListDimensionsInputBuilderState -> (ListDimensionsInputBuilderState, a)
}

instance Data.Functor.Functor ListDimensionsInputBuilder where
    fmap f (ListDimensionsInputBuilder g) =
        ListDimensionsInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListDimensionsInputBuilder where
    pure a = ListDimensionsInputBuilder (\s -> (s, a))
    (ListDimensionsInputBuilder f) <*> (ListDimensionsInputBuilder g) = ListDimensionsInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListDimensionsInputBuilder where
    (ListDimensionsInputBuilder f) >>= g = ListDimensionsInputBuilder (\s ->
        let (s', a) = f s
            (ListDimensionsInputBuilder h) = g a
        in h s')

setCount :: Data.Maybe.Maybe Integer -> ListDimensionsInputBuilder ()
setCount value =
   ListDimensionsInputBuilder (\s -> (s { countBuilderState = value }, ()))

setPage :: Data.Maybe.Maybe Integer -> ListDimensionsInputBuilder ()
setPage value =
   ListDimensionsInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setAll' :: Data.Maybe.Maybe Bool -> ListDimensionsInputBuilder ()
setAll' value =
   ListDimensionsInputBuilder (\s -> (s { all'BuilderState = value }, ()))

setWorkspaceId :: Data.Text.Text -> ListDimensionsInputBuilder ()
setWorkspaceId value =
   ListDimensionsInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> ListDimensionsInputBuilder ()
setOrgId value =
   ListDimensionsInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

build :: ListDimensionsInputBuilder () -> Data.Either.Either Data.Text.Text ListDimensionsInput
build builder = do
    let (st, _) = runListDimensionsInputBuilder builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListDimensionsInput.ListDimensionsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListDimensionsInput.ListDimensionsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    Data.Either.Right (ListDimensionsInput { 
        count = count',
        page = page',
        all' = all'',
        workspace_id = workspace_id',
        org_id = org_id'
    })



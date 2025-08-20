module Io.Superposition.Model.ListVersionsInput (
    setWorkspaceId,
    setOrgId,
    setCount,
    setPage,
    build,
    ListVersionsInputBuilder,
    ListVersionsInput,
    workspace_id,
    org_id,
    count,
    page
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

data ListVersionsInput = ListVersionsInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    count :: Data.Maybe.Maybe Integer,
    page :: Data.Maybe.Maybe Integer
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListVersionsInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a
        ]
    


instance Data.Aeson.FromJSON ListVersionsInput where
    parseJSON = Data.Aeson.withObject "ListVersionsInput" $ \v -> ListVersionsInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
    



data ListVersionsInputBuilderState = ListVersionsInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    countBuilderState :: Data.Maybe.Maybe Integer,
    pageBuilderState :: Data.Maybe.Maybe Integer
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListVersionsInputBuilderState
defaultBuilderState = ListVersionsInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing
}

newtype ListVersionsInputBuilder a = ListVersionsInputBuilder {
    runListVersionsInputBuilder :: ListVersionsInputBuilderState -> (ListVersionsInputBuilderState, a)
}

instance Data.Functor.Functor ListVersionsInputBuilder where
    fmap f (ListVersionsInputBuilder g) =
        ListVersionsInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListVersionsInputBuilder where
    pure a = ListVersionsInputBuilder (\s -> (s, a))
    (ListVersionsInputBuilder f) <*> (ListVersionsInputBuilder g) = ListVersionsInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListVersionsInputBuilder where
    (ListVersionsInputBuilder f) >>= g = ListVersionsInputBuilder (\s ->
        let (s', a) = f s
            (ListVersionsInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> ListVersionsInputBuilder ()
setWorkspaceId value =
   ListVersionsInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> ListVersionsInputBuilder ()
setOrgId value =
   ListVersionsInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setCount :: Data.Maybe.Maybe Integer -> ListVersionsInputBuilder ()
setCount value =
   ListVersionsInputBuilder (\s -> (s { countBuilderState = value }, ()))

setPage :: Data.Maybe.Maybe Integer -> ListVersionsInputBuilder ()
setPage value =
   ListVersionsInputBuilder (\s -> (s { pageBuilderState = value }, ()))

build :: ListVersionsInputBuilder () -> Data.Either.Either Data.Text.Text ListVersionsInput
build builder = do
    let (st, _) = runListVersionsInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVersionsInput.ListVersionsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVersionsInput.ListVersionsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    Data.Either.Right (ListVersionsInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        count = count',
        page = page'
    })



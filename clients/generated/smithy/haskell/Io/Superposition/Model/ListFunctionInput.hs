module Io.Superposition.Model.ListFunctionInput (
    setCount,
    setPage,
    setWorkspaceId,
    setOrgId,
    build,
    ListFunctionInputBuilder,
    ListFunctionInput,
    count,
    page,
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

data ListFunctionInput = ListFunctionInput {
    count :: Data.Maybe.Maybe Integer,
    page :: Data.Maybe.Maybe Integer,
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListFunctionInput where
    toJSON a = Data.Aeson.object [
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a
        ]
    


instance Data.Aeson.FromJSON ListFunctionInput where
    parseJSON = Data.Aeson.withObject "ListFunctionInput" $ \v -> ListFunctionInput
        Data.Functor.<$> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
    



data ListFunctionInputBuilderState = ListFunctionInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Integer,
    pageBuilderState :: Data.Maybe.Maybe Integer,
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListFunctionInputBuilderState
defaultBuilderState = ListFunctionInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing
}

newtype ListFunctionInputBuilder a = ListFunctionInputBuilder {
    runListFunctionInputBuilder :: ListFunctionInputBuilderState -> (ListFunctionInputBuilderState, a)
}

instance Data.Functor.Functor ListFunctionInputBuilder where
    fmap f (ListFunctionInputBuilder g) =
        ListFunctionInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListFunctionInputBuilder where
    pure a = ListFunctionInputBuilder (\s -> (s, a))
    (ListFunctionInputBuilder f) <*> (ListFunctionInputBuilder g) = ListFunctionInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListFunctionInputBuilder where
    (ListFunctionInputBuilder f) >>= g = ListFunctionInputBuilder (\s ->
        let (s', a) = f s
            (ListFunctionInputBuilder h) = g a
        in h s')

setCount :: Data.Maybe.Maybe Integer -> ListFunctionInputBuilder ()
setCount value =
   ListFunctionInputBuilder (\s -> (s { countBuilderState = value }, ()))

setPage :: Data.Maybe.Maybe Integer -> ListFunctionInputBuilder ()
setPage value =
   ListFunctionInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setWorkspaceId :: Data.Text.Text -> ListFunctionInputBuilder ()
setWorkspaceId value =
   ListFunctionInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> ListFunctionInputBuilder ()
setOrgId value =
   ListFunctionInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

build :: ListFunctionInputBuilder () -> Data.Either.Either Data.Text.Text ListFunctionInput
build builder = do
    let (st, _) = runListFunctionInputBuilder builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListFunctionInput.ListFunctionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListFunctionInput.ListFunctionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    Data.Either.Right (ListFunctionInput { 
        count = count',
        page = page',
        workspace_id = workspace_id',
        org_id = org_id'
    })



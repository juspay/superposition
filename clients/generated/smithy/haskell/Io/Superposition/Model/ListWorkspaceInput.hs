module Io.Superposition.Model.ListWorkspaceInput (
    setCount,
    setPage,
    setOrgId,
    build,
    ListWorkspaceInputBuilder,
    ListWorkspaceInput,
    count,
    page,
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

data ListWorkspaceInput = ListWorkspaceInput {
    count :: Data.Maybe.Maybe Integer,
    page :: Data.Maybe.Maybe Integer,
    org_id :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListWorkspaceInput where
    toJSON a = Data.Aeson.object [
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "org_id" Data.Aeson..= org_id a
        ]
    


instance Data.Aeson.FromJSON ListWorkspaceInput where
    parseJSON = Data.Aeson.withObject "ListWorkspaceInput" $ \v -> ListWorkspaceInput
        Data.Functor.<$> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
    



data ListWorkspaceInputBuilderState = ListWorkspaceInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Integer,
    pageBuilderState :: Data.Maybe.Maybe Integer,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListWorkspaceInputBuilderState
defaultBuilderState = ListWorkspaceInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing
}

newtype ListWorkspaceInputBuilder a = ListWorkspaceInputBuilder {
    runListWorkspaceInputBuilder :: ListWorkspaceInputBuilderState -> (ListWorkspaceInputBuilderState, a)
}

instance Data.Functor.Functor ListWorkspaceInputBuilder where
    fmap f (ListWorkspaceInputBuilder g) =
        ListWorkspaceInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListWorkspaceInputBuilder where
    pure a = ListWorkspaceInputBuilder (\s -> (s, a))
    (ListWorkspaceInputBuilder f) <*> (ListWorkspaceInputBuilder g) = ListWorkspaceInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListWorkspaceInputBuilder where
    (ListWorkspaceInputBuilder f) >>= g = ListWorkspaceInputBuilder (\s ->
        let (s', a) = f s
            (ListWorkspaceInputBuilder h) = g a
        in h s')

setCount :: Data.Maybe.Maybe Integer -> ListWorkspaceInputBuilder ()
setCount value =
   ListWorkspaceInputBuilder (\s -> (s { countBuilderState = value }, ()))

setPage :: Data.Maybe.Maybe Integer -> ListWorkspaceInputBuilder ()
setPage value =
   ListWorkspaceInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setOrgId :: Data.Text.Text -> ListWorkspaceInputBuilder ()
setOrgId value =
   ListWorkspaceInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

build :: ListWorkspaceInputBuilder () -> Data.Either.Either Data.Text.Text ListWorkspaceInput
build builder = do
    let (st, _) = runListWorkspaceInputBuilder builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWorkspaceInput.ListWorkspaceInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    Data.Either.Right (ListWorkspaceInput { 
        count = count',
        page = page',
        org_id = org_id'
    })



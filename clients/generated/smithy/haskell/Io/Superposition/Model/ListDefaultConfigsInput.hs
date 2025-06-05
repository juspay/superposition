module Io.Superposition.Model.ListDefaultConfigsInput (
    setCount,
    setPage,
    setWorkspaceId,
    setOrgId,
    build,
    ListDefaultConfigsInputBuilder,
    ListDefaultConfigsInput,
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

data ListDefaultConfigsInput = ListDefaultConfigsInput {
    count :: Data.Maybe.Maybe Integer,
    page :: Data.Maybe.Maybe Integer,
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListDefaultConfigsInput where
    toJSON a = Data.Aeson.object [
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a
        ]
    


instance Data.Aeson.FromJSON ListDefaultConfigsInput where
    parseJSON = Data.Aeson.withObject "ListDefaultConfigsInput" $ \v -> ListDefaultConfigsInput
        Data.Functor.<$> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
    



data ListDefaultConfigsInputBuilderState = ListDefaultConfigsInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Integer,
    pageBuilderState :: Data.Maybe.Maybe Integer,
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListDefaultConfigsInputBuilderState
defaultBuilderState = ListDefaultConfigsInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing
}

newtype ListDefaultConfigsInputBuilder a = ListDefaultConfigsInputBuilder {
    runListDefaultConfigsInputBuilder :: ListDefaultConfigsInputBuilderState -> (ListDefaultConfigsInputBuilderState, a)
}

instance Data.Functor.Functor ListDefaultConfigsInputBuilder where
    fmap f (ListDefaultConfigsInputBuilder g) =
        ListDefaultConfigsInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListDefaultConfigsInputBuilder where
    pure a = ListDefaultConfigsInputBuilder (\s -> (s, a))
    (ListDefaultConfigsInputBuilder f) <*> (ListDefaultConfigsInputBuilder g) = ListDefaultConfigsInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListDefaultConfigsInputBuilder where
    (ListDefaultConfigsInputBuilder f) >>= g = ListDefaultConfigsInputBuilder (\s ->
        let (s', a) = f s
            (ListDefaultConfigsInputBuilder h) = g a
        in h s')

setCount :: Data.Maybe.Maybe Integer -> ListDefaultConfigsInputBuilder ()
setCount value =
   ListDefaultConfigsInputBuilder (\s -> (s { countBuilderState = value }, ()))

setPage :: Data.Maybe.Maybe Integer -> ListDefaultConfigsInputBuilder ()
setPage value =
   ListDefaultConfigsInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setWorkspaceId :: Data.Text.Text -> ListDefaultConfigsInputBuilder ()
setWorkspaceId value =
   ListDefaultConfigsInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> ListDefaultConfigsInputBuilder ()
setOrgId value =
   ListDefaultConfigsInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

build :: ListDefaultConfigsInputBuilder () -> Data.Either.Either Data.Text.Text ListDefaultConfigsInput
build builder = do
    let (st, _) = runListDefaultConfigsInputBuilder builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListDefaultConfigsInput.ListDefaultConfigsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListDefaultConfigsInput.ListDefaultConfigsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    Data.Either.Right (ListDefaultConfigsInput { 
        count = count',
        page = page',
        workspace_id = workspace_id',
        org_id = org_id'
    })



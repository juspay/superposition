module Io.Superposition.Model.ListWebhookInput (
    setCount,
    setPage,
    setWorkspaceId,
    setOrgId,
    build,
    ListWebhookInputBuilder,
    ListWebhookInput,
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

data ListWebhookInput = ListWebhookInput {
    count :: Data.Maybe.Maybe Integer,
    page :: Data.Maybe.Maybe Integer,
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListWebhookInput where
    toJSON a = Data.Aeson.object [
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a
        ]
    


instance Data.Aeson.FromJSON ListWebhookInput where
    parseJSON = Data.Aeson.withObject "ListWebhookInput" $ \v -> ListWebhookInput
        Data.Functor.<$> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
    



data ListWebhookInputBuilderState = ListWebhookInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Integer,
    pageBuilderState :: Data.Maybe.Maybe Integer,
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListWebhookInputBuilderState
defaultBuilderState = ListWebhookInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing
}

newtype ListWebhookInputBuilder a = ListWebhookInputBuilder {
    runListWebhookInputBuilder :: ListWebhookInputBuilderState -> (ListWebhookInputBuilderState, a)
}

instance Data.Functor.Functor ListWebhookInputBuilder where
    fmap f (ListWebhookInputBuilder g) =
        ListWebhookInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListWebhookInputBuilder where
    pure a = ListWebhookInputBuilder (\s -> (s, a))
    (ListWebhookInputBuilder f) <*> (ListWebhookInputBuilder g) = ListWebhookInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListWebhookInputBuilder where
    (ListWebhookInputBuilder f) >>= g = ListWebhookInputBuilder (\s ->
        let (s', a) = f s
            (ListWebhookInputBuilder h) = g a
        in h s')

setCount :: Data.Maybe.Maybe Integer -> ListWebhookInputBuilder ()
setCount value =
   ListWebhookInputBuilder (\s -> (s { countBuilderState = value }, ()))

setPage :: Data.Maybe.Maybe Integer -> ListWebhookInputBuilder ()
setPage value =
   ListWebhookInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setWorkspaceId :: Data.Text.Text -> ListWebhookInputBuilder ()
setWorkspaceId value =
   ListWebhookInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> ListWebhookInputBuilder ()
setOrgId value =
   ListWebhookInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

build :: ListWebhookInputBuilder () -> Data.Either.Either Data.Text.Text ListWebhookInput
build builder = do
    let (st, _) = runListWebhookInputBuilder builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWebhookInput.ListWebhookInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWebhookInput.ListWebhookInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    Data.Either.Right (ListWebhookInput { 
        count = count',
        page = page',
        workspace_id = workspace_id',
        org_id = org_id'
    })



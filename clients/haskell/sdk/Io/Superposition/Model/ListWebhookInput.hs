module Io.Superposition.Model.ListWebhookInput (
    setCount,
    setPage,
    setAll',
    setWorkspaceId,
    setOrgId,
    build,
    ListWebhookInputBuilder,
    ListWebhookInput,
    count,
    page,
    all',
    workspace_id,
    org_id
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
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ListWebhookInput = ListWebhookInput {
    count :: Data.Maybe.Maybe Data.Int.Int32,
    page :: Data.Maybe.Maybe Data.Int.Int32,
    all' :: Data.Maybe.Maybe Bool,
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
        "all" Data.Aeson..= all' a,
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListWebhookInput

instance Data.Aeson.FromJSON ListWebhookInput where
    parseJSON = Data.Aeson.withObject "ListWebhookInput" $ \v -> ListWebhookInput
        Data.Functor.<$> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "all")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
    



data ListWebhookInputBuilderState = ListWebhookInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListWebhookInputBuilderState
defaultBuilderState = ListWebhookInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing
}

type ListWebhookInputBuilder = Control.Monad.State.Strict.State ListWebhookInputBuilderState

setCount :: Data.Maybe.Maybe Data.Int.Int32 -> ListWebhookInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> ListWebhookInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setAll' :: Data.Maybe.Maybe Bool -> ListWebhookInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setWorkspaceId :: Data.Text.Text -> ListWebhookInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListWebhookInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

build :: ListWebhookInputBuilder () -> Data.Either.Either Data.Text.Text ListWebhookInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWebhookInput.ListWebhookInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWebhookInput.ListWebhookInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    Data.Either.Right (ListWebhookInput { 
        count = count',
        page = page',
        all' = all'',
        workspace_id = workspace_id',
        org_id = org_id'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListWebhookInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "webhook"
            ]
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


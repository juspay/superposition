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

data ListVersionsInput = ListVersionsInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    count :: Data.Maybe.Maybe Data.Int.Int32,
    page :: Data.Maybe.Maybe Data.Int.Int32
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
    

instance Io.Superposition.Utility.SerializeBody ListVersionsInput

instance Data.Aeson.FromJSON ListVersionsInput where
    parseJSON = Data.Aeson.withObject "ListVersionsInput" $ \v -> ListVersionsInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
    



data ListVersionsInputBuilderState = ListVersionsInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32
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

type ListVersionsInputBuilder = Control.Monad.State.Strict.State ListVersionsInputBuilderState

setWorkspaceId :: Data.Text.Text -> ListVersionsInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListVersionsInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setCount :: Data.Maybe.Maybe Data.Int.Int32 -> ListVersionsInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> ListVersionsInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

build :: ListVersionsInputBuilder () -> Data.Either.Either Data.Text.Text ListVersionsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
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


instance Io.Superposition.Utility.IntoRequestBuilder ListVersionsInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "config",
            "versions"
            ]
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


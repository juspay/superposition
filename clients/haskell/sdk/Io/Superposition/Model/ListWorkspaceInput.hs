module Io.Superposition.Model.ListWorkspaceInput (
    setCount,
    setPage,
    setAll',
    setOrgId,
    build,
    ListWorkspaceInputBuilder,
    ListWorkspaceInput,
    count,
    page,
    all',
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

data ListWorkspaceInput = ListWorkspaceInput {
    count :: Data.Maybe.Maybe Data.Int.Int32,
    page :: Data.Maybe.Maybe Data.Int.Int32,
    all' :: Data.Maybe.Maybe Bool,
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
        "all" Data.Aeson..= all' a,
        "org_id" Data.Aeson..= org_id a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListWorkspaceInput

instance Data.Aeson.FromJSON ListWorkspaceInput where
    parseJSON = Data.Aeson.withObject "ListWorkspaceInput" $ \v -> ListWorkspaceInput
        Data.Functor.<$> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "all")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
    



data ListWorkspaceInputBuilderState = ListWorkspaceInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListWorkspaceInputBuilderState
defaultBuilderState = ListWorkspaceInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing
}

type ListWorkspaceInputBuilder = Control.Monad.State.Strict.State ListWorkspaceInputBuilderState

setCount :: Data.Maybe.Maybe Data.Int.Int32 -> ListWorkspaceInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> ListWorkspaceInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setAll' :: Data.Maybe.Maybe Bool -> ListWorkspaceInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setOrgId :: Data.Text.Text -> ListWorkspaceInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

build :: ListWorkspaceInputBuilder () -> Data.Either.Either Data.Text.Text ListWorkspaceInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWorkspaceInput.ListWorkspaceInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    Data.Either.Right (ListWorkspaceInput { 
        count = count',
        page = page',
        all' = all'',
        org_id = org_id'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListWorkspaceInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "workspaces"
            ]
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


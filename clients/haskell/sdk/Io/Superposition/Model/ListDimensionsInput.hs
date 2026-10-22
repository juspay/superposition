module Io.Superposition.Model.ListDimensionsInput (
    setCount,
    setPage,
    setAll',
    setWorkspaceId,
    setOrgId,
    setUserAgent,
    build,
    ListDimensionsInputBuilder,
    ListDimensionsInput,
    count,
    page,
    all',
    workspace_id,
    org_id,
    user_agent
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

data ListDimensionsInput = ListDimensionsInput {
    count :: Data.Maybe.Maybe Data.Int.Int32,
    page :: Data.Maybe.Maybe Data.Int.Int32,
    all' :: Data.Maybe.Maybe Bool,
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    user_agent :: Data.Maybe.Maybe Data.Text.Text
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
        "org_id" Data.Aeson..= org_id a,
        "user_agent" Data.Aeson..= user_agent a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListDimensionsInput

instance Data.Aeson.FromJSON ListDimensionsInput where
    parseJSON = Data.Aeson.withObject "ListDimensionsInput" $ \v -> ListDimensionsInput
        Data.Functor.<$> (v Data.Aeson..:? "count")
        Control.Applicative.<*> (v Data.Aeson..:? "page")
        Control.Applicative.<*> (v Data.Aeson..:? "all")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "user_agent")
    



data ListDimensionsInputBuilderState = ListDimensionsInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    user_agentBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListDimensionsInputBuilderState
defaultBuilderState = ListDimensionsInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    user_agentBuilderState = Data.Maybe.Nothing
}

type ListDimensionsInputBuilder = Control.Monad.State.Strict.State ListDimensionsInputBuilderState

setCount :: Data.Maybe.Maybe Data.Int.Int32 -> ListDimensionsInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> ListDimensionsInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setAll' :: Data.Maybe.Maybe Bool -> ListDimensionsInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setWorkspaceId :: Data.Text.Text -> ListDimensionsInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListDimensionsInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setUserAgent :: Data.Maybe.Maybe Data.Text.Text -> ListDimensionsInputBuilder ()
setUserAgent value =
   Control.Monad.State.Strict.modify (\s -> (s { user_agentBuilderState = value }))

build :: ListDimensionsInputBuilder () -> Data.Either.Either Data.Text.Text ListDimensionsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListDimensionsInput.ListDimensionsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListDimensionsInput.ListDimensionsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    user_agent' <- Data.Either.Right (user_agentBuilderState st)
    Data.Either.Right (ListDimensionsInput { 
        count = count',
        page = page',
        all' = all'',
        workspace_id = workspace_id',
        org_id = org_id',
        user_agent = user_agent'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListDimensionsInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "dimension"
            ]
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serHeader "user-agent" (user_agent self)
        


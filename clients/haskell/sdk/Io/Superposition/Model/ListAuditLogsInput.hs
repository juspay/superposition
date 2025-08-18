module Io.Superposition.Model.ListAuditLogsInput (
    setWorkspaceId,
    setOrgId,
    setCount,
    setPage,
    setAll',
    setFromDate,
    setToDate,
    setTables,
    setAction,
    setUsername,
    build,
    ListAuditLogsInputBuilder,
    ListAuditLogsInput,
    workspace_id,
    org_id,
    count,
    page,
    all',
    from_date,
    to_date,
    tables,
    action,
    username
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
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ListAuditLogsInput = ListAuditLogsInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    count :: Data.Maybe.Maybe Data.Int.Int32,
    page :: Data.Maybe.Maybe Data.Int.Int32,
    all' :: Data.Maybe.Maybe Bool,
    from_date :: Data.Maybe.Maybe Data.Time.UTCTime,
    to_date :: Data.Maybe.Maybe Data.Time.UTCTime,
    tables :: Data.Maybe.Maybe Data.Text.Text,
    action :: Data.Maybe.Maybe Data.Text.Text,
    username :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListAuditLogsInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "all" Data.Aeson..= all' a,
        "from_date" Data.Aeson..= from_date a,
        "to_date" Data.Aeson..= to_date a,
        "tables" Data.Aeson..= tables a,
        "action" Data.Aeson..= action a,
        "username" Data.Aeson..= username a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListAuditLogsInput

instance Data.Aeson.FromJSON ListAuditLogsInput where
    parseJSON = Data.Aeson.withObject "ListAuditLogsInput" $ \v -> ListAuditLogsInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "all")
        Control.Applicative.<*> (v Data.Aeson..: "from_date")
        Control.Applicative.<*> (v Data.Aeson..: "to_date")
        Control.Applicative.<*> (v Data.Aeson..: "tables")
        Control.Applicative.<*> (v Data.Aeson..: "action")
        Control.Applicative.<*> (v Data.Aeson..: "username")
    



data ListAuditLogsInputBuilderState = ListAuditLogsInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    from_dateBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    to_dateBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    tablesBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    actionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    usernameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListAuditLogsInputBuilderState
defaultBuilderState = ListAuditLogsInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    from_dateBuilderState = Data.Maybe.Nothing,
    to_dateBuilderState = Data.Maybe.Nothing,
    tablesBuilderState = Data.Maybe.Nothing,
    actionBuilderState = Data.Maybe.Nothing,
    usernameBuilderState = Data.Maybe.Nothing
}

type ListAuditLogsInputBuilder = Control.Monad.State.Strict.State ListAuditLogsInputBuilderState

setWorkspaceId :: Data.Text.Text -> ListAuditLogsInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListAuditLogsInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setCount :: Data.Maybe.Maybe Data.Int.Int32 -> ListAuditLogsInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> ListAuditLogsInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setAll' :: Data.Maybe.Maybe Bool -> ListAuditLogsInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setFromDate :: Data.Maybe.Maybe Data.Time.UTCTime -> ListAuditLogsInputBuilder ()
setFromDate value =
   Control.Monad.State.Strict.modify (\s -> (s { from_dateBuilderState = value }))

setToDate :: Data.Maybe.Maybe Data.Time.UTCTime -> ListAuditLogsInputBuilder ()
setToDate value =
   Control.Monad.State.Strict.modify (\s -> (s { to_dateBuilderState = value }))

setTables :: Data.Maybe.Maybe Data.Text.Text -> ListAuditLogsInputBuilder ()
setTables value =
   Control.Monad.State.Strict.modify (\s -> (s { tablesBuilderState = value }))

setAction :: Data.Maybe.Maybe Data.Text.Text -> ListAuditLogsInputBuilder ()
setAction value =
   Control.Monad.State.Strict.modify (\s -> (s { actionBuilderState = value }))

setUsername :: Data.Maybe.Maybe Data.Text.Text -> ListAuditLogsInputBuilder ()
setUsername value =
   Control.Monad.State.Strict.modify (\s -> (s { usernameBuilderState = value }))

build :: ListAuditLogsInputBuilder () -> Data.Either.Either Data.Text.Text ListAuditLogsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListAuditLogsInput.ListAuditLogsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListAuditLogsInput.ListAuditLogsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    from_date' <- Data.Either.Right (from_dateBuilderState st)
    to_date' <- Data.Either.Right (to_dateBuilderState st)
    tables' <- Data.Either.Right (tablesBuilderState st)
    action' <- Data.Either.Right (actionBuilderState st)
    username' <- Data.Either.Right (usernameBuilderState st)
    Data.Either.Right (ListAuditLogsInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        count = count',
        page = page',
        all' = all'',
        from_date = from_date',
        to_date = to_date',
        tables = tables',
        action = action',
        username = username'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListAuditLogsInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "audit"
            ]
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "table" (tables self)
        Io.Superposition.Utility.serQuery "from_date" (from_date self)
        Io.Superposition.Utility.serQuery "to_date" (to_date self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "action" (action self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serQuery "username" (username self)
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


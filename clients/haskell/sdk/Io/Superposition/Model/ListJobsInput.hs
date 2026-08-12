module Io.Superposition.Model.ListJobsInput (
    setCount,
    setPage,
    setAll',
    setWorkspaceId,
    setOrgId,
    setStatus,
    setJobType,
    build,
    ListJobsInputBuilder,
    ListJobsInput,
    count,
    page,
    all',
    workspace_id,
    org_id,
    status,
    job_type
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
import qualified Io.Superposition.Model.BackgroundJobStatus
import qualified Io.Superposition.Model.BackgroundJobType
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ListJobsInput = ListJobsInput {
    count :: Data.Maybe.Maybe Data.Int.Int32,
    page :: Data.Maybe.Maybe Data.Int.Int32,
    all' :: Data.Maybe.Maybe Bool,
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    status :: Data.Maybe.Maybe Io.Superposition.Model.BackgroundJobStatus.BackgroundJobStatus,
    job_type :: Data.Maybe.Maybe Io.Superposition.Model.BackgroundJobType.BackgroundJobType
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListJobsInput where
    toJSON a = Data.Aeson.object [
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "all" Data.Aeson..= all' a,
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "status" Data.Aeson..= status a,
        "job_type" Data.Aeson..= job_type a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListJobsInput

instance Data.Aeson.FromJSON ListJobsInput where
    parseJSON = Data.Aeson.withObject "ListJobsInput" $ \v -> ListJobsInput
        Data.Functor.<$> (v Data.Aeson..:? "count")
        Control.Applicative.<*> (v Data.Aeson..:? "page")
        Control.Applicative.<*> (v Data.Aeson..:? "all")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "status")
        Control.Applicative.<*> (v Data.Aeson..:? "job_type")
    



data ListJobsInputBuilderState = ListJobsInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    all'BuilderState :: Data.Maybe.Maybe Bool,
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.BackgroundJobStatus.BackgroundJobStatus,
    job_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.BackgroundJobType.BackgroundJobType
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListJobsInputBuilderState
defaultBuilderState = ListJobsInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing,
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    statusBuilderState = Data.Maybe.Nothing,
    job_typeBuilderState = Data.Maybe.Nothing
}

type ListJobsInputBuilder = Control.Monad.State.Strict.State ListJobsInputBuilderState

setCount :: Data.Maybe.Maybe Data.Int.Int32 -> ListJobsInputBuilder ()
setCount value =
   Control.Monad.State.Strict.modify (\s -> (s { countBuilderState = value }))

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> ListJobsInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setAll' :: Data.Maybe.Maybe Bool -> ListJobsInputBuilder ()
setAll' value =
   Control.Monad.State.Strict.modify (\s -> (s { all'BuilderState = value }))

setWorkspaceId :: Data.Text.Text -> ListJobsInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ListJobsInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setStatus :: Data.Maybe.Maybe Io.Superposition.Model.BackgroundJobStatus.BackgroundJobStatus -> ListJobsInputBuilder ()
setStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { statusBuilderState = value }))

setJobType :: Data.Maybe.Maybe Io.Superposition.Model.BackgroundJobType.BackgroundJobType -> ListJobsInputBuilder ()
setJobType value =
   Control.Monad.State.Strict.modify (\s -> (s { job_typeBuilderState = value }))

build :: ListJobsInputBuilder () -> Data.Either.Either Data.Text.Text ListJobsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListJobsInput.ListJobsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListJobsInput.ListJobsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    status' <- Data.Either.Right (statusBuilderState st)
    job_type' <- Data.Either.Right (job_typeBuilderState st)
    Data.Either.Right (ListJobsInput { 
        count = count',
        page = page',
        all' = all'',
        workspace_id = workspace_id',
        org_id = org_id',
        status = status',
        job_type = job_type'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ListJobsInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "jobs"
            ]
        Io.Superposition.Utility.serQuery "all" (all' self)
        Io.Superposition.Utility.serQuery "job_type" (job_type self)
        Io.Superposition.Utility.serQuery "count" (count self)
        Io.Superposition.Utility.serQuery "page" (page self)
        Io.Superposition.Utility.serQuery "status" (status self)
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


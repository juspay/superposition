module Io.Superposition.Model.JobDetailResponse (
    setId',
    setKronosJobId,
    setDescription,
    setJobType,
    setStatus,
    setName,
    setProgress,
    setWorkspaceSchema,
    setCreatedAt,
    setLogs,
    setExecution,
    build,
    JobDetailResponseBuilder,
    JobDetailResponse,
    id',
    kronos_job_id,
    description,
    job_type,
    status,
    name,
    progress,
    workspace_schema,
    created_at,
    logs,
    execution
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
import qualified Io.Superposition.Model.BackgroundJobStatus
import qualified Io.Superposition.Model.BackgroundJobType
import qualified Io.Superposition.Model.ExecutionDetails
import qualified Io.Superposition.Utility

data JobDetailResponse = JobDetailResponse {
    id' :: Data.Text.Text,
    kronos_job_id :: Data.Text.Text,
    description :: Data.Text.Text,
    job_type :: Io.Superposition.Model.BackgroundJobType.BackgroundJobType,
    status :: Io.Superposition.Model.BackgroundJobStatus.BackgroundJobStatus,
    name :: Data.Text.Text,
    progress :: Data.Int.Int32,
    workspace_schema :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    logs :: Data.Aeson.Value,
    execution :: Data.Maybe.Maybe Io.Superposition.Model.ExecutionDetails.ExecutionDetails
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON JobDetailResponse where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "kronos_job_id" Data.Aeson..= kronos_job_id a,
        "description" Data.Aeson..= description a,
        "job_type" Data.Aeson..= job_type a,
        "status" Data.Aeson..= status a,
        "name" Data.Aeson..= name a,
        "progress" Data.Aeson..= progress a,
        "workspace_schema" Data.Aeson..= workspace_schema a,
        "created_at" Data.Aeson..= created_at a,
        "logs" Data.Aeson..= logs a,
        "execution" Data.Aeson..= execution a
        ]
    

instance Io.Superposition.Utility.SerializeBody JobDetailResponse

instance Data.Aeson.FromJSON JobDetailResponse where
    parseJSON = Data.Aeson.withObject "JobDetailResponse" $ \v -> JobDetailResponse
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "kronos_job_id")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "job_type")
        Control.Applicative.<*> (v Data.Aeson..: "status")
        Control.Applicative.<*> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "progress")
        Control.Applicative.<*> (v Data.Aeson..: "workspace_schema")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "logs")
        Control.Applicative.<*> (v Data.Aeson..:? "execution")
    



data JobDetailResponseBuilderState = JobDetailResponseBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    kronos_job_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    job_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.BackgroundJobType.BackgroundJobType,
    statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.BackgroundJobStatus.BackgroundJobStatus,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    progressBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    workspace_schemaBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    logsBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    executionBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ExecutionDetails.ExecutionDetails
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: JobDetailResponseBuilderState
defaultBuilderState = JobDetailResponseBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    kronos_job_idBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    job_typeBuilderState = Data.Maybe.Nothing,
    statusBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    progressBuilderState = Data.Maybe.Nothing,
    workspace_schemaBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    logsBuilderState = Data.Maybe.Nothing,
    executionBuilderState = Data.Maybe.Nothing
}

type JobDetailResponseBuilder = Control.Monad.State.Strict.State JobDetailResponseBuilderState

setId' :: Data.Text.Text -> JobDetailResponseBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setKronosJobId :: Data.Text.Text -> JobDetailResponseBuilder ()
setKronosJobId value =
   Control.Monad.State.Strict.modify (\s -> (s { kronos_job_idBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> JobDetailResponseBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setJobType :: Io.Superposition.Model.BackgroundJobType.BackgroundJobType -> JobDetailResponseBuilder ()
setJobType value =
   Control.Monad.State.Strict.modify (\s -> (s { job_typeBuilderState = Data.Maybe.Just value }))

setStatus :: Io.Superposition.Model.BackgroundJobStatus.BackgroundJobStatus -> JobDetailResponseBuilder ()
setStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { statusBuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> JobDetailResponseBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setProgress :: Data.Int.Int32 -> JobDetailResponseBuilder ()
setProgress value =
   Control.Monad.State.Strict.modify (\s -> (s { progressBuilderState = Data.Maybe.Just value }))

setWorkspaceSchema :: Data.Text.Text -> JobDetailResponseBuilder ()
setWorkspaceSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_schemaBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> JobDetailResponseBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setLogs :: Data.Aeson.Value -> JobDetailResponseBuilder ()
setLogs value =
   Control.Monad.State.Strict.modify (\s -> (s { logsBuilderState = Data.Maybe.Just value }))

setExecution :: Data.Maybe.Maybe Io.Superposition.Model.ExecutionDetails.ExecutionDetails -> JobDetailResponseBuilder ()
setExecution value =
   Control.Monad.State.Strict.modify (\s -> (s { executionBuilderState = value }))

build :: JobDetailResponseBuilder () -> Data.Either.Either Data.Text.Text JobDetailResponse
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.JobDetailResponse.JobDetailResponse.id' is a required property.") Data.Either.Right (id'BuilderState st)
    kronos_job_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.JobDetailResponse.JobDetailResponse.kronos_job_id is a required property.") Data.Either.Right (kronos_job_idBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.JobDetailResponse.JobDetailResponse.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    job_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.JobDetailResponse.JobDetailResponse.job_type is a required property.") Data.Either.Right (job_typeBuilderState st)
    status' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.JobDetailResponse.JobDetailResponse.status is a required property.") Data.Either.Right (statusBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.JobDetailResponse.JobDetailResponse.name is a required property.") Data.Either.Right (nameBuilderState st)
    progress' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.JobDetailResponse.JobDetailResponse.progress is a required property.") Data.Either.Right (progressBuilderState st)
    workspace_schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.JobDetailResponse.JobDetailResponse.workspace_schema is a required property.") Data.Either.Right (workspace_schemaBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.JobDetailResponse.JobDetailResponse.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    logs' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.JobDetailResponse.JobDetailResponse.logs is a required property.") Data.Either.Right (logsBuilderState st)
    execution' <- Data.Either.Right (executionBuilderState st)
    Data.Either.Right (JobDetailResponse { 
        id' = id'',
        kronos_job_id = kronos_job_id',
        description = description',
        job_type = job_type',
        status = status',
        name = name',
        progress = progress',
        workspace_schema = workspace_schema',
        created_at = created_at',
        logs = logs',
        execution = execution'
    })



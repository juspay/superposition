module Io.Superposition.Model.GetJobOutput (
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
    GetJobOutputBuilder,
    GetJobOutput,
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
import qualified Network.HTTP.Types

data GetJobOutput = GetJobOutput {
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

instance Data.Aeson.ToJSON GetJobOutput where
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
    

instance Io.Superposition.Utility.SerializeBody GetJobOutput

instance Data.Aeson.FromJSON GetJobOutput where
    parseJSON = Data.Aeson.withObject "GetJobOutput" $ \v -> GetJobOutput
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
    



data GetJobOutputBuilderState = GetJobOutputBuilderState {
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

defaultBuilderState :: GetJobOutputBuilderState
defaultBuilderState = GetJobOutputBuilderState {
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

type GetJobOutputBuilder = Control.Monad.State.Strict.State GetJobOutputBuilderState

setId' :: Data.Text.Text -> GetJobOutputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setKronosJobId :: Data.Text.Text -> GetJobOutputBuilder ()
setKronosJobId value =
   Control.Monad.State.Strict.modify (\s -> (s { kronos_job_idBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> GetJobOutputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setJobType :: Io.Superposition.Model.BackgroundJobType.BackgroundJobType -> GetJobOutputBuilder ()
setJobType value =
   Control.Monad.State.Strict.modify (\s -> (s { job_typeBuilderState = Data.Maybe.Just value }))

setStatus :: Io.Superposition.Model.BackgroundJobStatus.BackgroundJobStatus -> GetJobOutputBuilder ()
setStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { statusBuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> GetJobOutputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setProgress :: Data.Int.Int32 -> GetJobOutputBuilder ()
setProgress value =
   Control.Monad.State.Strict.modify (\s -> (s { progressBuilderState = Data.Maybe.Just value }))

setWorkspaceSchema :: Data.Text.Text -> GetJobOutputBuilder ()
setWorkspaceSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_schemaBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> GetJobOutputBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setLogs :: Data.Aeson.Value -> GetJobOutputBuilder ()
setLogs value =
   Control.Monad.State.Strict.modify (\s -> (s { logsBuilderState = Data.Maybe.Just value }))

setExecution :: Data.Maybe.Maybe Io.Superposition.Model.ExecutionDetails.ExecutionDetails -> GetJobOutputBuilder ()
setExecution value =
   Control.Monad.State.Strict.modify (\s -> (s { executionBuilderState = value }))

build :: GetJobOutputBuilder () -> Data.Either.Either Data.Text.Text GetJobOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetJobOutput.GetJobOutput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    kronos_job_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetJobOutput.GetJobOutput.kronos_job_id is a required property.") Data.Either.Right (kronos_job_idBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetJobOutput.GetJobOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    job_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetJobOutput.GetJobOutput.job_type is a required property.") Data.Either.Right (job_typeBuilderState st)
    status' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetJobOutput.GetJobOutput.status is a required property.") Data.Either.Right (statusBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetJobOutput.GetJobOutput.name is a required property.") Data.Either.Right (nameBuilderState st)
    progress' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetJobOutput.GetJobOutput.progress is a required property.") Data.Either.Right (progressBuilderState st)
    workspace_schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetJobOutput.GetJobOutput.workspace_schema is a required property.") Data.Either.Right (workspace_schemaBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetJobOutput.GetJobOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    logs' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetJobOutput.GetJobOutput.logs is a required property.") Data.Either.Right (logsBuilderState st)
    execution' <- Data.Either.Right (executionBuilderState st)
    Data.Either.Right (GetJobOutput { 
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


instance Io.Superposition.Utility.FromResponseParser GetJobOutput where
    expectedStatus = (Network.HTTP.Types.mkStatus 200 "")
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "job_type"
        var1 <- Io.Superposition.Utility.deSerField "execution"
        var2 <- Io.Superposition.Utility.deSerField "kronos_job_id"
        var3 <- Io.Superposition.Utility.deSerField "name"
        var4 <- Io.Superposition.Utility.deSerField "description"
        var5 <- Io.Superposition.Utility.deSerField "progress"
        var6 <- Io.Superposition.Utility.deSerField "created_at"
        var7 <- Io.Superposition.Utility.deSerField "id"
        var8 <- Io.Superposition.Utility.deSerField "workspace_schema"
        var9 <- Io.Superposition.Utility.deSerField "logs"
        var10 <- Io.Superposition.Utility.deSerField "status"
        pure $ GetJobOutput {
            id' = var7,
            kronos_job_id = var2,
            description = var4,
            job_type = var0,
            status = var10,
            name = var3,
            progress = var5,
            workspace_schema = var8,
            created_at = var6,
            logs = var9,
            execution = var1
        }


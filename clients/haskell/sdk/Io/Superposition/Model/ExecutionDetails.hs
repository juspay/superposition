module Io.Superposition.Model.ExecutionDetails (
    setAttemptCount,
    setMaxAttempts,
    setStartedAt,
    setCompletedAt,
    setDurationMs,
    setExecutionStatus,
    build,
    ExecutionDetailsBuilder,
    ExecutionDetails,
    attempt_count,
    max_attempts,
    started_at,
    completed_at,
    duration_ms,
    execution_status
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

data ExecutionDetails = ExecutionDetails {
    attempt_count :: Data.Maybe.Maybe Data.Int.Int64,
    max_attempts :: Data.Maybe.Maybe Data.Int.Int64,
    started_at :: Data.Maybe.Maybe Data.Time.UTCTime,
    completed_at :: Data.Maybe.Maybe Data.Time.UTCTime,
    duration_ms :: Data.Maybe.Maybe Data.Int.Int64,
    execution_status :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ExecutionDetails where
    toJSON a = Data.Aeson.object [
        "attempt_count" Data.Aeson..= attempt_count a,
        "max_attempts" Data.Aeson..= max_attempts a,
        "started_at" Data.Aeson..= started_at a,
        "completed_at" Data.Aeson..= completed_at a,
        "duration_ms" Data.Aeson..= duration_ms a,
        "execution_status" Data.Aeson..= execution_status a
        ]
    

instance Io.Superposition.Utility.SerializeBody ExecutionDetails

instance Data.Aeson.FromJSON ExecutionDetails where
    parseJSON = Data.Aeson.withObject "ExecutionDetails" $ \v -> ExecutionDetails
        Data.Functor.<$> (v Data.Aeson..:? "attempt_count")
        Control.Applicative.<*> (v Data.Aeson..:? "max_attempts")
        Control.Applicative.<*> (v Data.Aeson..:? "started_at")
        Control.Applicative.<*> (v Data.Aeson..:? "completed_at")
        Control.Applicative.<*> (v Data.Aeson..:? "duration_ms")
        Control.Applicative.<*> (v Data.Aeson..:? "execution_status")
    



data ExecutionDetailsBuilderState = ExecutionDetailsBuilderState {
    attempt_countBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    max_attemptsBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    started_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    completed_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    duration_msBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    execution_statusBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ExecutionDetailsBuilderState
defaultBuilderState = ExecutionDetailsBuilderState {
    attempt_countBuilderState = Data.Maybe.Nothing,
    max_attemptsBuilderState = Data.Maybe.Nothing,
    started_atBuilderState = Data.Maybe.Nothing,
    completed_atBuilderState = Data.Maybe.Nothing,
    duration_msBuilderState = Data.Maybe.Nothing,
    execution_statusBuilderState = Data.Maybe.Nothing
}

type ExecutionDetailsBuilder = Control.Monad.State.Strict.State ExecutionDetailsBuilderState

setAttemptCount :: Data.Maybe.Maybe Data.Int.Int64 -> ExecutionDetailsBuilder ()
setAttemptCount value =
   Control.Monad.State.Strict.modify (\s -> (s { attempt_countBuilderState = value }))

setMaxAttempts :: Data.Maybe.Maybe Data.Int.Int64 -> ExecutionDetailsBuilder ()
setMaxAttempts value =
   Control.Monad.State.Strict.modify (\s -> (s { max_attemptsBuilderState = value }))

setStartedAt :: Data.Maybe.Maybe Data.Time.UTCTime -> ExecutionDetailsBuilder ()
setStartedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { started_atBuilderState = value }))

setCompletedAt :: Data.Maybe.Maybe Data.Time.UTCTime -> ExecutionDetailsBuilder ()
setCompletedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { completed_atBuilderState = value }))

setDurationMs :: Data.Maybe.Maybe Data.Int.Int64 -> ExecutionDetailsBuilder ()
setDurationMs value =
   Control.Monad.State.Strict.modify (\s -> (s { duration_msBuilderState = value }))

setExecutionStatus :: Data.Maybe.Maybe Data.Text.Text -> ExecutionDetailsBuilder ()
setExecutionStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { execution_statusBuilderState = value }))

build :: ExecutionDetailsBuilder () -> Data.Either.Either Data.Text.Text ExecutionDetails
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    attempt_count' <- Data.Either.Right (attempt_countBuilderState st)
    max_attempts' <- Data.Either.Right (max_attemptsBuilderState st)
    started_at' <- Data.Either.Right (started_atBuilderState st)
    completed_at' <- Data.Either.Right (completed_atBuilderState st)
    duration_ms' <- Data.Either.Right (duration_msBuilderState st)
    execution_status' <- Data.Either.Right (execution_statusBuilderState st)
    Data.Either.Right (ExecutionDetails { 
        attempt_count = attempt_count',
        max_attempts = max_attempts',
        started_at = started_at',
        completed_at = completed_at',
        duration_ms = duration_ms',
        execution_status = execution_status'
    })



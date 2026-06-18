module Io.Superposition.Model.WorkspaceLock (
    setLockId,
    setOperation,
    setLockedBy,
    setAcquiredAt,
    setExpiresAt,
    build,
    WorkspaceLockBuilder,
    WorkspaceLock,
    lock_id,
    operation,
    locked_by,
    acquired_at,
    expires_at
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

data WorkspaceLock = WorkspaceLock {
    lock_id :: Data.Text.Text,
    operation :: Data.Text.Text,
    locked_by :: Data.Text.Text,
    acquired_at :: Data.Time.UTCTime,
    expires_at :: Data.Time.UTCTime
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WorkspaceLock where
    toJSON a = Data.Aeson.object [
        "lock_id" Data.Aeson..= lock_id a,
        "operation" Data.Aeson..= operation a,
        "locked_by" Data.Aeson..= locked_by a,
        "acquired_at" Data.Aeson..= acquired_at a,
        "expires_at" Data.Aeson..= expires_at a
        ]
    

instance Io.Superposition.Utility.SerializeBody WorkspaceLock

instance Data.Aeson.FromJSON WorkspaceLock where
    parseJSON = Data.Aeson.withObject "WorkspaceLock" $ \v -> WorkspaceLock
        Data.Functor.<$> (v Data.Aeson..: "lock_id")
        Control.Applicative.<*> (v Data.Aeson..: "operation")
        Control.Applicative.<*> (v Data.Aeson..: "locked_by")
        Control.Applicative.<*> (v Data.Aeson..: "acquired_at")
        Control.Applicative.<*> (v Data.Aeson..: "expires_at")
    



data WorkspaceLockBuilderState = WorkspaceLockBuilderState {
    lock_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    operationBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    locked_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    acquired_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    expires_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WorkspaceLockBuilderState
defaultBuilderState = WorkspaceLockBuilderState {
    lock_idBuilderState = Data.Maybe.Nothing,
    operationBuilderState = Data.Maybe.Nothing,
    locked_byBuilderState = Data.Maybe.Nothing,
    acquired_atBuilderState = Data.Maybe.Nothing,
    expires_atBuilderState = Data.Maybe.Nothing
}

type WorkspaceLockBuilder = Control.Monad.State.Strict.State WorkspaceLockBuilderState

setLockId :: Data.Text.Text -> WorkspaceLockBuilder ()
setLockId value =
   Control.Monad.State.Strict.modify (\s -> (s { lock_idBuilderState = Data.Maybe.Just value }))

setOperation :: Data.Text.Text -> WorkspaceLockBuilder ()
setOperation value =
   Control.Monad.State.Strict.modify (\s -> (s { operationBuilderState = Data.Maybe.Just value }))

setLockedBy :: Data.Text.Text -> WorkspaceLockBuilder ()
setLockedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { locked_byBuilderState = Data.Maybe.Just value }))

setAcquiredAt :: Data.Time.UTCTime -> WorkspaceLockBuilder ()
setAcquiredAt value =
   Control.Monad.State.Strict.modify (\s -> (s { acquired_atBuilderState = Data.Maybe.Just value }))

setExpiresAt :: Data.Time.UTCTime -> WorkspaceLockBuilder ()
setExpiresAt value =
   Control.Monad.State.Strict.modify (\s -> (s { expires_atBuilderState = Data.Maybe.Just value }))

build :: WorkspaceLockBuilder () -> Data.Either.Either Data.Text.Text WorkspaceLock
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    lock_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WorkspaceLock.WorkspaceLock.lock_id is a required property.") Data.Either.Right (lock_idBuilderState st)
    operation' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WorkspaceLock.WorkspaceLock.operation is a required property.") Data.Either.Right (operationBuilderState st)
    locked_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WorkspaceLock.WorkspaceLock.locked_by is a required property.") Data.Either.Right (locked_byBuilderState st)
    acquired_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WorkspaceLock.WorkspaceLock.acquired_at is a required property.") Data.Either.Right (acquired_atBuilderState st)
    expires_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WorkspaceLock.WorkspaceLock.expires_at is a required property.") Data.Either.Right (expires_atBuilderState st)
    Data.Either.Right (WorkspaceLock { 
        lock_id = lock_id',
        operation = operation',
        locked_by = locked_by',
        acquired_at = acquired_at',
        expires_at = expires_at'
    })



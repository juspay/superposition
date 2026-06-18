module Io.Superposition.Model.WorkspaceLockConflict (
    setMessage,
    setLock,
    build,
    WorkspaceLockConflictBuilder,
    WorkspaceLockConflict,
    message,
    lock
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.WorkspaceLock
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data WorkspaceLockConflict = WorkspaceLockConflict {
    message :: Data.Text.Text,
    lock :: Io.Superposition.Model.WorkspaceLock.WorkspaceLock
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WorkspaceLockConflict where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a,
        "lock" Data.Aeson..= lock a
        ]
    

instance Io.Superposition.Utility.SerializeBody WorkspaceLockConflict

instance Data.Aeson.FromJSON WorkspaceLockConflict where
    parseJSON = Data.Aeson.withObject "WorkspaceLockConflict" $ \v -> WorkspaceLockConflict
        Data.Functor.<$> (v Data.Aeson..: "message")
        Control.Applicative.<*> (v Data.Aeson..: "lock")
    



data WorkspaceLockConflictBuilderState = WorkspaceLockConflictBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    lockBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.WorkspaceLock.WorkspaceLock
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WorkspaceLockConflictBuilderState
defaultBuilderState = WorkspaceLockConflictBuilderState {
    messageBuilderState = Data.Maybe.Nothing,
    lockBuilderState = Data.Maybe.Nothing
}

type WorkspaceLockConflictBuilder = Control.Monad.State.Strict.State WorkspaceLockConflictBuilderState

setMessage :: Data.Text.Text -> WorkspaceLockConflictBuilder ()
setMessage value =
   Control.Monad.State.Strict.modify (\s -> (s { messageBuilderState = Data.Maybe.Just value }))

setLock :: Io.Superposition.Model.WorkspaceLock.WorkspaceLock -> WorkspaceLockConflictBuilder ()
setLock value =
   Control.Monad.State.Strict.modify (\s -> (s { lockBuilderState = Data.Maybe.Just value }))

build :: WorkspaceLockConflictBuilder () -> Data.Either.Either Data.Text.Text WorkspaceLockConflict
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WorkspaceLockConflict.WorkspaceLockConflict.message is a required property.") Data.Either.Right (messageBuilderState st)
    lock' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WorkspaceLockConflict.WorkspaceLockConflict.lock is a required property.") Data.Either.Right (lockBuilderState st)
    Data.Either.Right (WorkspaceLockConflict { 
        message = message',
        lock = lock'
    })


instance Io.Superposition.Utility.FromResponseParser WorkspaceLockConflict where
    expectedStatus = (Network.HTTP.Types.mkStatus 409 "")
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "lock"
        var1 <- Io.Superposition.Utility.deSerField "message"
        pure $ WorkspaceLockConflict {
            message = var1,
            lock = var0
        }


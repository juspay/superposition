module Io.Superposition.Model.InternalServerError (
    setMessage,
    build,
    InternalServerErrorBuilder,
    InternalServerError,
    message
) where
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data InternalServerError = InternalServerError {
    message :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON InternalServerError where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    

instance Io.Superposition.Utility.SerializeBody InternalServerError

instance Data.Aeson.FromJSON InternalServerError where
    parseJSON = Data.Aeson.withObject "InternalServerError" $ \v -> InternalServerError
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data InternalServerErrorBuilderState = InternalServerErrorBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: InternalServerErrorBuilderState
defaultBuilderState = InternalServerErrorBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

type InternalServerErrorBuilder = Control.Monad.State.Strict.State InternalServerErrorBuilderState

setMessage :: Data.Maybe.Maybe Data.Text.Text -> InternalServerErrorBuilder ()
setMessage value =
   Control.Monad.State.Strict.modify (\s -> (s { messageBuilderState = value }))

build :: InternalServerErrorBuilder () -> Data.Either.Either Data.Text.Text InternalServerError
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    message' <- Data.Either.Right (messageBuilderState st)
    Data.Either.Right (InternalServerError { 
        message = message'
    })


instance Io.Superposition.Utility.FromResponseParser InternalServerError where
    expectedStatus = Network.HTTP.Types.status500
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "message"
        pure $ InternalServerError {
            message = var0
        }


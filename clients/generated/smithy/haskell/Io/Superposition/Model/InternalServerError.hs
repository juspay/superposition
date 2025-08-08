module Io.Superposition.Model.InternalServerError (
    setMessage,
    build,
    InternalServerErrorBuilder,
    InternalServerError,
    message
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

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

newtype InternalServerErrorBuilder a = InternalServerErrorBuilder {
    runInternalServerErrorBuilder :: InternalServerErrorBuilderState -> (InternalServerErrorBuilderState, a)
}

instance Data.Functor.Functor InternalServerErrorBuilder where
    fmap f (InternalServerErrorBuilder g) =
        InternalServerErrorBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative InternalServerErrorBuilder where
    pure a = InternalServerErrorBuilder (\s -> (s, a))
    (InternalServerErrorBuilder f) <*> (InternalServerErrorBuilder g) = InternalServerErrorBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad InternalServerErrorBuilder where
    (InternalServerErrorBuilder f) >>= g = InternalServerErrorBuilder (\s ->
        let (s', a) = f s
            (InternalServerErrorBuilder h) = g a
        in h s')

setMessage :: Data.Maybe.Maybe Data.Text.Text -> InternalServerErrorBuilder ()
setMessage value =
   InternalServerErrorBuilder (\s -> (s { messageBuilderState = value }, ()))

build :: InternalServerErrorBuilder () -> Data.Either.Either Data.Text.Text InternalServerError
build builder = do
    let (st, _) = runInternalServerErrorBuilder builder defaultBuilderState
    message' <- Data.Either.Right (messageBuilderState st)
    Data.Either.Right (InternalServerError { 
        message = message'
    })



module Io.Superposition.Model.DeleteContextOutput (
    build,
    DeleteContextOutputBuilder,
    DeleteContextOutput
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data DeleteContextOutput = DeleteContextOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteContextOutput where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON DeleteContextOutput where
    parseJSON = Data.Aeson.withObject "DeleteContextOutput" $ \_ -> pure $ DeleteContextOutput



data DeleteContextOutputBuilderState = DeleteContextOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteContextOutputBuilderState
defaultBuilderState = DeleteContextOutputBuilderState {
}

newtype DeleteContextOutputBuilder a = DeleteContextOutputBuilder {
    runDeleteContextOutputBuilder :: DeleteContextOutputBuilderState -> (DeleteContextOutputBuilderState, a)
}

instance Data.Functor.Functor DeleteContextOutputBuilder where
    fmap f (DeleteContextOutputBuilder g) =
        DeleteContextOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DeleteContextOutputBuilder where
    pure a = DeleteContextOutputBuilder (\s -> (s, a))
    (DeleteContextOutputBuilder f) <*> (DeleteContextOutputBuilder g) = DeleteContextOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DeleteContextOutputBuilder where
    (DeleteContextOutputBuilder f) >>= g = DeleteContextOutputBuilder (\s ->
        let (s', a) = f s
            (DeleteContextOutputBuilder h) = g a
        in h s')


build :: DeleteContextOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteContextOutput
build builder = do
    let (st, _) = runDeleteContextOutputBuilder builder defaultBuilderState
    Data.Either.Right (DeleteContextOutput { 
    })



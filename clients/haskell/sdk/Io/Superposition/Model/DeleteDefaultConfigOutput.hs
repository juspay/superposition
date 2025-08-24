module Io.Superposition.Model.DeleteDefaultConfigOutput (
    build,
    DeleteDefaultConfigOutputBuilder,
    DeleteDefaultConfigOutput
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

data DeleteDefaultConfigOutput = DeleteDefaultConfigOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteDefaultConfigOutput where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON DeleteDefaultConfigOutput where
    parseJSON = Data.Aeson.withObject "DeleteDefaultConfigOutput" $ \_ -> pure $ DeleteDefaultConfigOutput



data DeleteDefaultConfigOutputBuilderState = DeleteDefaultConfigOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteDefaultConfigOutputBuilderState
defaultBuilderState = DeleteDefaultConfigOutputBuilderState {
}

newtype DeleteDefaultConfigOutputBuilder a = DeleteDefaultConfigOutputBuilder {
    runDeleteDefaultConfigOutputBuilder :: DeleteDefaultConfigOutputBuilderState -> (DeleteDefaultConfigOutputBuilderState, a)
}

instance Data.Functor.Functor DeleteDefaultConfigOutputBuilder where
    fmap f (DeleteDefaultConfigOutputBuilder g) =
        DeleteDefaultConfigOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DeleteDefaultConfigOutputBuilder where
    pure a = DeleteDefaultConfigOutputBuilder (\s -> (s, a))
    (DeleteDefaultConfigOutputBuilder f) <*> (DeleteDefaultConfigOutputBuilder g) = DeleteDefaultConfigOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DeleteDefaultConfigOutputBuilder where
    (DeleteDefaultConfigOutputBuilder f) >>= g = DeleteDefaultConfigOutputBuilder (\s ->
        let (s', a) = f s
            (DeleteDefaultConfigOutputBuilder h) = g a
        in h s')


build :: DeleteDefaultConfigOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteDefaultConfigOutput
build builder = do
    let (st, _) = runDeleteDefaultConfigOutputBuilder builder defaultBuilderState
    Data.Either.Right (DeleteDefaultConfigOutput { 
    })



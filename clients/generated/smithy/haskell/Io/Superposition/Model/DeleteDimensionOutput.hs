module Io.Superposition.Model.DeleteDimensionOutput (
    build,
    DeleteDimensionOutputBuilder,
    DeleteDimensionOutput
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

data DeleteDimensionOutput = DeleteDimensionOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteDimensionOutput where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON DeleteDimensionOutput where
    parseJSON = Data.Aeson.withObject "DeleteDimensionOutput" $ \_ -> pure $ DeleteDimensionOutput



data DeleteDimensionOutputBuilderState = DeleteDimensionOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteDimensionOutputBuilderState
defaultBuilderState = DeleteDimensionOutputBuilderState {
}

newtype DeleteDimensionOutputBuilder a = DeleteDimensionOutputBuilder {
    runDeleteDimensionOutputBuilder :: DeleteDimensionOutputBuilderState -> (DeleteDimensionOutputBuilderState, a)
}

instance Data.Functor.Functor DeleteDimensionOutputBuilder where
    fmap f (DeleteDimensionOutputBuilder g) =
        DeleteDimensionOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DeleteDimensionOutputBuilder where
    pure a = DeleteDimensionOutputBuilder (\s -> (s, a))
    (DeleteDimensionOutputBuilder f) <*> (DeleteDimensionOutputBuilder g) = DeleteDimensionOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DeleteDimensionOutputBuilder where
    (DeleteDimensionOutputBuilder f) >>= g = DeleteDimensionOutputBuilder (\s ->
        let (s', a) = f s
            (DeleteDimensionOutputBuilder h) = g a
        in h s')


build :: DeleteDimensionOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteDimensionOutput
build builder = do
    let (st, _) = runDeleteDimensionOutputBuilder builder defaultBuilderState
    Data.Either.Right (DeleteDimensionOutput { 
    })



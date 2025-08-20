module Io.Superposition.Model.FunctionNotFound (
    build,
    FunctionNotFoundBuilder,
    FunctionNotFound
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

data FunctionNotFound = FunctionNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON FunctionNotFound where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON FunctionNotFound where
    parseJSON = Data.Aeson.withObject "FunctionNotFound" $ \_ -> pure $ FunctionNotFound



data FunctionNotFoundBuilderState = FunctionNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: FunctionNotFoundBuilderState
defaultBuilderState = FunctionNotFoundBuilderState {
}

newtype FunctionNotFoundBuilder a = FunctionNotFoundBuilder {
    runFunctionNotFoundBuilder :: FunctionNotFoundBuilderState -> (FunctionNotFoundBuilderState, a)
}

instance Data.Functor.Functor FunctionNotFoundBuilder where
    fmap f (FunctionNotFoundBuilder g) =
        FunctionNotFoundBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative FunctionNotFoundBuilder where
    pure a = FunctionNotFoundBuilder (\s -> (s, a))
    (FunctionNotFoundBuilder f) <*> (FunctionNotFoundBuilder g) = FunctionNotFoundBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad FunctionNotFoundBuilder where
    (FunctionNotFoundBuilder f) >>= g = FunctionNotFoundBuilder (\s ->
        let (s', a) = f s
            (FunctionNotFoundBuilder h) = g a
        in h s')


build :: FunctionNotFoundBuilder () -> Data.Either.Either Data.Text.Text FunctionNotFound
build builder = do
    let (st, _) = runFunctionNotFoundBuilder builder defaultBuilderState
    Data.Either.Right (FunctionNotFound { 
    })



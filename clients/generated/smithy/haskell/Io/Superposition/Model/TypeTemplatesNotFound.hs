module Io.Superposition.Model.TypeTemplatesNotFound (
    build,
    TypeTemplatesNotFoundBuilder,
    TypeTemplatesNotFound
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

data TypeTemplatesNotFound = TypeTemplatesNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TypeTemplatesNotFound where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON TypeTemplatesNotFound where
    parseJSON = Data.Aeson.withObject "TypeTemplatesNotFound" $ \_ -> pure $ TypeTemplatesNotFound



data TypeTemplatesNotFoundBuilderState = TypeTemplatesNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TypeTemplatesNotFoundBuilderState
defaultBuilderState = TypeTemplatesNotFoundBuilderState {
}

newtype TypeTemplatesNotFoundBuilder a = TypeTemplatesNotFoundBuilder {
    runTypeTemplatesNotFoundBuilder :: TypeTemplatesNotFoundBuilderState -> (TypeTemplatesNotFoundBuilderState, a)
}

instance Data.Functor.Functor TypeTemplatesNotFoundBuilder where
    fmap f (TypeTemplatesNotFoundBuilder g) =
        TypeTemplatesNotFoundBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TypeTemplatesNotFoundBuilder where
    pure a = TypeTemplatesNotFoundBuilder (\s -> (s, a))
    (TypeTemplatesNotFoundBuilder f) <*> (TypeTemplatesNotFoundBuilder g) = TypeTemplatesNotFoundBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TypeTemplatesNotFoundBuilder where
    (TypeTemplatesNotFoundBuilder f) >>= g = TypeTemplatesNotFoundBuilder (\s ->
        let (s', a) = f s
            (TypeTemplatesNotFoundBuilder h) = g a
        in h s')


build :: TypeTemplatesNotFoundBuilder () -> Data.Either.Either Data.Text.Text TypeTemplatesNotFound
build builder = do
    let (st, _) = runTypeTemplatesNotFoundBuilder builder defaultBuilderState
    Data.Either.Right (TypeTemplatesNotFound { 
    })



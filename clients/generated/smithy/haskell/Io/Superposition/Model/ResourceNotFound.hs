module Io.Superposition.Model.ResourceNotFound (
    build,
    ResourceNotFoundBuilder,
    ResourceNotFound
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

data ResourceNotFound = ResourceNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ResourceNotFound where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON ResourceNotFound where
    parseJSON = Data.Aeson.withObject "ResourceNotFound" $ \_ -> pure $ ResourceNotFound



data ResourceNotFoundBuilderState = ResourceNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ResourceNotFoundBuilderState
defaultBuilderState = ResourceNotFoundBuilderState {
}

newtype ResourceNotFoundBuilder a = ResourceNotFoundBuilder {
    runResourceNotFoundBuilder :: ResourceNotFoundBuilderState -> (ResourceNotFoundBuilderState, a)
}

instance Data.Functor.Functor ResourceNotFoundBuilder where
    fmap f (ResourceNotFoundBuilder g) =
        ResourceNotFoundBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ResourceNotFoundBuilder where
    pure a = ResourceNotFoundBuilder (\s -> (s, a))
    (ResourceNotFoundBuilder f) <*> (ResourceNotFoundBuilder g) = ResourceNotFoundBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ResourceNotFoundBuilder where
    (ResourceNotFoundBuilder f) >>= g = ResourceNotFoundBuilder (\s ->
        let (s', a) = f s
            (ResourceNotFoundBuilder h) = g a
        in h s')


build :: ResourceNotFoundBuilder () -> Data.Either.Either Data.Text.Text ResourceNotFound
build builder = do
    let (st, _) = runResourceNotFoundBuilder builder defaultBuilderState
    Data.Either.Right (ResourceNotFound { 
    })



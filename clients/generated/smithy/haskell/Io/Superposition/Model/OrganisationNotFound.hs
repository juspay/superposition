module Io.Superposition.Model.OrganisationNotFound (
    build,
    OrganisationNotFoundBuilder,
    OrganisationNotFound
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

data OrganisationNotFound = OrganisationNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON OrganisationNotFound where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON OrganisationNotFound where
    parseJSON = Data.Aeson.withObject "OrganisationNotFound" $ \_ -> pure $ OrganisationNotFound



data OrganisationNotFoundBuilderState = OrganisationNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: OrganisationNotFoundBuilderState
defaultBuilderState = OrganisationNotFoundBuilderState {
}

newtype OrganisationNotFoundBuilder a = OrganisationNotFoundBuilder {
    runOrganisationNotFoundBuilder :: OrganisationNotFoundBuilderState -> (OrganisationNotFoundBuilderState, a)
}

instance Data.Functor.Functor OrganisationNotFoundBuilder where
    fmap f (OrganisationNotFoundBuilder g) =
        OrganisationNotFoundBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative OrganisationNotFoundBuilder where
    pure a = OrganisationNotFoundBuilder (\s -> (s, a))
    (OrganisationNotFoundBuilder f) <*> (OrganisationNotFoundBuilder g) = OrganisationNotFoundBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad OrganisationNotFoundBuilder where
    (OrganisationNotFoundBuilder f) >>= g = OrganisationNotFoundBuilder (\s ->
        let (s', a) = f s
            (OrganisationNotFoundBuilder h) = g a
        in h s')


build :: OrganisationNotFoundBuilder () -> Data.Either.Either Data.Text.Text OrganisationNotFound
build builder = do
    let (st, _) = runOrganisationNotFoundBuilder builder defaultBuilderState
    Data.Either.Right (OrganisationNotFound { 
    })



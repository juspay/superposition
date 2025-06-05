module Io.Superposition.Model.GetOrganisationInput (
    setId',
    build,
    GetOrganisationInputBuilder,
    GetOrganisationInput,
    id'
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

data GetOrganisationInput = GetOrganisationInput {
    id' :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetOrganisationInput where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a
        ]
    


instance Data.Aeson.FromJSON GetOrganisationInput where
    parseJSON = Data.Aeson.withObject "GetOrganisationInput" $ \v -> GetOrganisationInput
        Data.Functor.<$> (v Data.Aeson..: "id")
    



data GetOrganisationInputBuilderState = GetOrganisationInputBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetOrganisationInputBuilderState
defaultBuilderState = GetOrganisationInputBuilderState {
    id'BuilderState = Data.Maybe.Nothing
}

newtype GetOrganisationInputBuilder a = GetOrganisationInputBuilder {
    runGetOrganisationInputBuilder :: GetOrganisationInputBuilderState -> (GetOrganisationInputBuilderState, a)
}

instance Data.Functor.Functor GetOrganisationInputBuilder where
    fmap f (GetOrganisationInputBuilder g) =
        GetOrganisationInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetOrganisationInputBuilder where
    pure a = GetOrganisationInputBuilder (\s -> (s, a))
    (GetOrganisationInputBuilder f) <*> (GetOrganisationInputBuilder g) = GetOrganisationInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetOrganisationInputBuilder where
    (GetOrganisationInputBuilder f) >>= g = GetOrganisationInputBuilder (\s ->
        let (s', a) = f s
            (GetOrganisationInputBuilder h) = g a
        in h s')

setId' :: Data.Text.Text -> GetOrganisationInputBuilder ()
setId' value =
   GetOrganisationInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

build :: GetOrganisationInputBuilder () -> Data.Either.Either Data.Text.Text GetOrganisationInput
build builder = do
    let (st, _) = runGetOrganisationInputBuilder builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetOrganisationInput.GetOrganisationInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    Data.Either.Right (GetOrganisationInput { 
        id' = id''
    })



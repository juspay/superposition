module Io.Superposition.Model.ListOrganisationInput (
    setCount,
    setPage,
    setAll',
    build,
    ListOrganisationInputBuilder,
    ListOrganisationInput,
    count,
    page,
    all'
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

data ListOrganisationInput = ListOrganisationInput {
    count :: Data.Maybe.Maybe Integer,
    page :: Data.Maybe.Maybe Integer,
    all' :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListOrganisationInput where
    toJSON a = Data.Aeson.object [
        "count" Data.Aeson..= count a,
        "page" Data.Aeson..= page a,
        "all" Data.Aeson..= all' a
        ]
    


instance Data.Aeson.FromJSON ListOrganisationInput where
    parseJSON = Data.Aeson.withObject "ListOrganisationInput" $ \v -> ListOrganisationInput
        Data.Functor.<$> (v Data.Aeson..: "count")
        Control.Applicative.<*> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "all")
    



data ListOrganisationInputBuilderState = ListOrganisationInputBuilderState {
    countBuilderState :: Data.Maybe.Maybe Integer,
    pageBuilderState :: Data.Maybe.Maybe Integer,
    all'BuilderState :: Data.Maybe.Maybe Bool
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListOrganisationInputBuilderState
defaultBuilderState = ListOrganisationInputBuilderState {
    countBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    all'BuilderState = Data.Maybe.Nothing
}

newtype ListOrganisationInputBuilder a = ListOrganisationInputBuilder {
    runListOrganisationInputBuilder :: ListOrganisationInputBuilderState -> (ListOrganisationInputBuilderState, a)
}

instance Data.Functor.Functor ListOrganisationInputBuilder where
    fmap f (ListOrganisationInputBuilder g) =
        ListOrganisationInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListOrganisationInputBuilder where
    pure a = ListOrganisationInputBuilder (\s -> (s, a))
    (ListOrganisationInputBuilder f) <*> (ListOrganisationInputBuilder g) = ListOrganisationInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListOrganisationInputBuilder where
    (ListOrganisationInputBuilder f) >>= g = ListOrganisationInputBuilder (\s ->
        let (s', a) = f s
            (ListOrganisationInputBuilder h) = g a
        in h s')

setCount :: Data.Maybe.Maybe Integer -> ListOrganisationInputBuilder ()
setCount value =
   ListOrganisationInputBuilder (\s -> (s { countBuilderState = value }, ()))

setPage :: Data.Maybe.Maybe Integer -> ListOrganisationInputBuilder ()
setPage value =
   ListOrganisationInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setAll' :: Data.Maybe.Maybe Bool -> ListOrganisationInputBuilder ()
setAll' value =
   ListOrganisationInputBuilder (\s -> (s { all'BuilderState = value }, ()))

build :: ListOrganisationInputBuilder () -> Data.Either.Either Data.Text.Text ListOrganisationInput
build builder = do
    let (st, _) = runListOrganisationInputBuilder builder defaultBuilderState
    count' <- Data.Either.Right (countBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    all'' <- Data.Either.Right (all'BuilderState st)
    Data.Either.Right (ListOrganisationInput { 
        count = count',
        page = page',
        all' = all''
    })



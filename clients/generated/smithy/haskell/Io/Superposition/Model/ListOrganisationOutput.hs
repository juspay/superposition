module Io.Superposition.Model.ListOrganisationOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    ListOrganisationOutputBuilder,
    ListOrganisationOutput,
    total_pages,
    total_items,
    data'
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
import qualified Io.Superposition.Model.OrganisationResponse

data ListOrganisationOutput = ListOrganisationOutput {
    total_pages :: Data.Maybe.Maybe Integer,
    total_items :: Data.Maybe.Maybe Integer,
    data' :: Data.Maybe.Maybe ([] Io.Superposition.Model.OrganisationResponse.OrganisationResponse)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListOrganisationOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON ListOrganisationOutput where
    parseJSON = Data.Aeson.withObject "ListOrganisationOutput" $ \v -> ListOrganisationOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListOrganisationOutputBuilderState = ListOrganisationOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Integer,
    total_itemsBuilderState :: Data.Maybe.Maybe Integer,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.OrganisationResponse.OrganisationResponse)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListOrganisationOutputBuilderState
defaultBuilderState = ListOrganisationOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

newtype ListOrganisationOutputBuilder a = ListOrganisationOutputBuilder {
    runListOrganisationOutputBuilder :: ListOrganisationOutputBuilderState -> (ListOrganisationOutputBuilderState, a)
}

instance Data.Functor.Functor ListOrganisationOutputBuilder where
    fmap f (ListOrganisationOutputBuilder g) =
        ListOrganisationOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListOrganisationOutputBuilder where
    pure a = ListOrganisationOutputBuilder (\s -> (s, a))
    (ListOrganisationOutputBuilder f) <*> (ListOrganisationOutputBuilder g) = ListOrganisationOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListOrganisationOutputBuilder where
    (ListOrganisationOutputBuilder f) >>= g = ListOrganisationOutputBuilder (\s ->
        let (s', a) = f s
            (ListOrganisationOutputBuilder h) = g a
        in h s')

setTotalPages :: Data.Maybe.Maybe Integer -> ListOrganisationOutputBuilder ()
setTotalPages value =
   ListOrganisationOutputBuilder (\s -> (s { total_pagesBuilderState = value }, ()))

setTotalItems :: Data.Maybe.Maybe Integer -> ListOrganisationOutputBuilder ()
setTotalItems value =
   ListOrganisationOutputBuilder (\s -> (s { total_itemsBuilderState = value }, ()))

setData' :: Data.Maybe.Maybe ([] Io.Superposition.Model.OrganisationResponse.OrganisationResponse) -> ListOrganisationOutputBuilder ()
setData' value =
   ListOrganisationOutputBuilder (\s -> (s { data'BuilderState = value }, ()))

build :: ListOrganisationOutputBuilder () -> Data.Either.Either Data.Text.Text ListOrganisationOutput
build builder = do
    let (st, _) = runListOrganisationOutputBuilder builder defaultBuilderState
    total_pages' <- Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListOrganisationOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })



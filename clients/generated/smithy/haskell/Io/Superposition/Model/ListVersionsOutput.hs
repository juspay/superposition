module Io.Superposition.Model.ListVersionsOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    ListVersionsOutputBuilder,
    ListVersionsOutput,
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
import qualified Io.Superposition.Model.ListVersionsMember

data ListVersionsOutput = ListVersionsOutput {
    total_pages :: Integer,
    total_items :: Integer,
    data' :: [] Io.Superposition.Model.ListVersionsMember.ListVersionsMember
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListVersionsOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON ListVersionsOutput where
    parseJSON = Data.Aeson.withObject "ListVersionsOutput" $ \v -> ListVersionsOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListVersionsOutputBuilderState = ListVersionsOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Integer,
    total_itemsBuilderState :: Data.Maybe.Maybe Integer,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ListVersionsMember.ListVersionsMember)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListVersionsOutputBuilderState
defaultBuilderState = ListVersionsOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

newtype ListVersionsOutputBuilder a = ListVersionsOutputBuilder {
    runListVersionsOutputBuilder :: ListVersionsOutputBuilderState -> (ListVersionsOutputBuilderState, a)
}

instance Data.Functor.Functor ListVersionsOutputBuilder where
    fmap f (ListVersionsOutputBuilder g) =
        ListVersionsOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListVersionsOutputBuilder where
    pure a = ListVersionsOutputBuilder (\s -> (s, a))
    (ListVersionsOutputBuilder f) <*> (ListVersionsOutputBuilder g) = ListVersionsOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListVersionsOutputBuilder where
    (ListVersionsOutputBuilder f) >>= g = ListVersionsOutputBuilder (\s ->
        let (s', a) = f s
            (ListVersionsOutputBuilder h) = g a
        in h s')

setTotalPages :: Integer -> ListVersionsOutputBuilder ()
setTotalPages value =
   ListVersionsOutputBuilder (\s -> (s { total_pagesBuilderState = Data.Maybe.Just value }, ()))

setTotalItems :: Integer -> ListVersionsOutputBuilder ()
setTotalItems value =
   ListVersionsOutputBuilder (\s -> (s { total_itemsBuilderState = Data.Maybe.Just value }, ()))

setData' :: [] Io.Superposition.Model.ListVersionsMember.ListVersionsMember -> ListVersionsOutputBuilder ()
setData' value =
   ListVersionsOutputBuilder (\s -> (s { data'BuilderState = Data.Maybe.Just value }, ()))

build :: ListVersionsOutputBuilder () -> Data.Either.Either Data.Text.Text ListVersionsOutput
build builder = do
    let (st, _) = runListVersionsOutputBuilder builder defaultBuilderState
    total_pages' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVersionsOutput.ListVersionsOutput.total_pages is a required property.") Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVersionsOutput.ListVersionsOutput.total_items is a required property.") Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVersionsOutput.ListVersionsOutput.data' is a required property.") Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListVersionsOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })



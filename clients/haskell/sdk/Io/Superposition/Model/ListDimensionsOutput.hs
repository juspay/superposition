module Io.Superposition.Model.ListDimensionsOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    ListDimensionsOutputBuilder,
    ListDimensionsOutput,
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
import qualified Io.Superposition.Model.DimensionExt

data ListDimensionsOutput = ListDimensionsOutput {
    total_pages :: Data.Maybe.Maybe Integer,
    total_items :: Data.Maybe.Maybe Integer,
    data' :: Data.Maybe.Maybe ([] Io.Superposition.Model.DimensionExt.DimensionExt)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListDimensionsOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON ListDimensionsOutput where
    parseJSON = Data.Aeson.withObject "ListDimensionsOutput" $ \v -> ListDimensionsOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListDimensionsOutputBuilderState = ListDimensionsOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Integer,
    total_itemsBuilderState :: Data.Maybe.Maybe Integer,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.DimensionExt.DimensionExt)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListDimensionsOutputBuilderState
defaultBuilderState = ListDimensionsOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

newtype ListDimensionsOutputBuilder a = ListDimensionsOutputBuilder {
    runListDimensionsOutputBuilder :: ListDimensionsOutputBuilderState -> (ListDimensionsOutputBuilderState, a)
}

instance Data.Functor.Functor ListDimensionsOutputBuilder where
    fmap f (ListDimensionsOutputBuilder g) =
        ListDimensionsOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListDimensionsOutputBuilder where
    pure a = ListDimensionsOutputBuilder (\s -> (s, a))
    (ListDimensionsOutputBuilder f) <*> (ListDimensionsOutputBuilder g) = ListDimensionsOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListDimensionsOutputBuilder where
    (ListDimensionsOutputBuilder f) >>= g = ListDimensionsOutputBuilder (\s ->
        let (s', a) = f s
            (ListDimensionsOutputBuilder h) = g a
        in h s')

setTotalPages :: Data.Maybe.Maybe Integer -> ListDimensionsOutputBuilder ()
setTotalPages value =
   ListDimensionsOutputBuilder (\s -> (s { total_pagesBuilderState = value }, ()))

setTotalItems :: Data.Maybe.Maybe Integer -> ListDimensionsOutputBuilder ()
setTotalItems value =
   ListDimensionsOutputBuilder (\s -> (s { total_itemsBuilderState = value }, ()))

setData' :: Data.Maybe.Maybe ([] Io.Superposition.Model.DimensionExt.DimensionExt) -> ListDimensionsOutputBuilder ()
setData' value =
   ListDimensionsOutputBuilder (\s -> (s { data'BuilderState = value }, ()))

build :: ListDimensionsOutputBuilder () -> Data.Either.Either Data.Text.Text ListDimensionsOutput
build builder = do
    let (st, _) = runListDimensionsOutputBuilder builder defaultBuilderState
    total_pages' <- Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListDimensionsOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })



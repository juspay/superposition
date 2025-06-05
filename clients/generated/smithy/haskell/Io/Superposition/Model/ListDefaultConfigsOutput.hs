module Io.Superposition.Model.ListDefaultConfigsOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    ListDefaultConfigsOutputBuilder,
    ListDefaultConfigsOutput,
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
import qualified Io.Superposition.Model.DefaultConfigFull

data ListDefaultConfigsOutput = ListDefaultConfigsOutput {
    total_pages :: Data.Maybe.Maybe Integer,
    total_items :: Data.Maybe.Maybe Integer,
    data' :: Data.Maybe.Maybe ([] Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListDefaultConfigsOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON ListDefaultConfigsOutput where
    parseJSON = Data.Aeson.withObject "ListDefaultConfigsOutput" $ \v -> ListDefaultConfigsOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListDefaultConfigsOutputBuilderState = ListDefaultConfigsOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Integer,
    total_itemsBuilderState :: Data.Maybe.Maybe Integer,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListDefaultConfigsOutputBuilderState
defaultBuilderState = ListDefaultConfigsOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

newtype ListDefaultConfigsOutputBuilder a = ListDefaultConfigsOutputBuilder {
    runListDefaultConfigsOutputBuilder :: ListDefaultConfigsOutputBuilderState -> (ListDefaultConfigsOutputBuilderState, a)
}

instance Data.Functor.Functor ListDefaultConfigsOutputBuilder where
    fmap f (ListDefaultConfigsOutputBuilder g) =
        ListDefaultConfigsOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListDefaultConfigsOutputBuilder where
    pure a = ListDefaultConfigsOutputBuilder (\s -> (s, a))
    (ListDefaultConfigsOutputBuilder f) <*> (ListDefaultConfigsOutputBuilder g) = ListDefaultConfigsOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListDefaultConfigsOutputBuilder where
    (ListDefaultConfigsOutputBuilder f) >>= g = ListDefaultConfigsOutputBuilder (\s ->
        let (s', a) = f s
            (ListDefaultConfigsOutputBuilder h) = g a
        in h s')

setTotalPages :: Data.Maybe.Maybe Integer -> ListDefaultConfigsOutputBuilder ()
setTotalPages value =
   ListDefaultConfigsOutputBuilder (\s -> (s { total_pagesBuilderState = value }, ()))

setTotalItems :: Data.Maybe.Maybe Integer -> ListDefaultConfigsOutputBuilder ()
setTotalItems value =
   ListDefaultConfigsOutputBuilder (\s -> (s { total_itemsBuilderState = value }, ()))

setData' :: Data.Maybe.Maybe ([] Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull) -> ListDefaultConfigsOutputBuilder ()
setData' value =
   ListDefaultConfigsOutputBuilder (\s -> (s { data'BuilderState = value }, ()))

build :: ListDefaultConfigsOutputBuilder () -> Data.Either.Either Data.Text.Text ListDefaultConfigsOutput
build builder = do
    let (st, _) = runListDefaultConfigsOutputBuilder builder defaultBuilderState
    total_pages' <- Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListDefaultConfigsOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })



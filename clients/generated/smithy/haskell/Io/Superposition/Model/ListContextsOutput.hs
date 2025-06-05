module Io.Superposition.Model.ListContextsOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    ListContextsOutputBuilder,
    ListContextsOutput,
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
import qualified Io.Superposition.Model.ContextFull

data ListContextsOutput = ListContextsOutput {
    total_pages :: Data.Maybe.Maybe Integer,
    total_items :: Data.Maybe.Maybe Integer,
    data' :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextFull.ContextFull)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListContextsOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON ListContextsOutput where
    parseJSON = Data.Aeson.withObject "ListContextsOutput" $ \v -> ListContextsOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListContextsOutputBuilderState = ListContextsOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Integer,
    total_itemsBuilderState :: Data.Maybe.Maybe Integer,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextFull.ContextFull)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListContextsOutputBuilderState
defaultBuilderState = ListContextsOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

newtype ListContextsOutputBuilder a = ListContextsOutputBuilder {
    runListContextsOutputBuilder :: ListContextsOutputBuilderState -> (ListContextsOutputBuilderState, a)
}

instance Data.Functor.Functor ListContextsOutputBuilder where
    fmap f (ListContextsOutputBuilder g) =
        ListContextsOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListContextsOutputBuilder where
    pure a = ListContextsOutputBuilder (\s -> (s, a))
    (ListContextsOutputBuilder f) <*> (ListContextsOutputBuilder g) = ListContextsOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListContextsOutputBuilder where
    (ListContextsOutputBuilder f) >>= g = ListContextsOutputBuilder (\s ->
        let (s', a) = f s
            (ListContextsOutputBuilder h) = g a
        in h s')

setTotalPages :: Data.Maybe.Maybe Integer -> ListContextsOutputBuilder ()
setTotalPages value =
   ListContextsOutputBuilder (\s -> (s { total_pagesBuilderState = value }, ()))

setTotalItems :: Data.Maybe.Maybe Integer -> ListContextsOutputBuilder ()
setTotalItems value =
   ListContextsOutputBuilder (\s -> (s { total_itemsBuilderState = value }, ()))

setData' :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextFull.ContextFull) -> ListContextsOutputBuilder ()
setData' value =
   ListContextsOutputBuilder (\s -> (s { data'BuilderState = value }, ()))

build :: ListContextsOutputBuilder () -> Data.Either.Either Data.Text.Text ListContextsOutput
build builder = do
    let (st, _) = runListContextsOutputBuilder builder defaultBuilderState
    total_pages' <- Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListContextsOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })



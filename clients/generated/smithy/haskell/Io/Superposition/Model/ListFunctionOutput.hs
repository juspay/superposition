module Io.Superposition.Model.ListFunctionOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    ListFunctionOutputBuilder,
    ListFunctionOutput,
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
import qualified Io.Superposition.Model.FunctionResponse

data ListFunctionOutput = ListFunctionOutput {
    total_pages :: Data.Maybe.Maybe Integer,
    total_items :: Data.Maybe.Maybe Integer,
    data' :: Data.Maybe.Maybe ([] Io.Superposition.Model.FunctionResponse.FunctionResponse)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListFunctionOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON ListFunctionOutput where
    parseJSON = Data.Aeson.withObject "ListFunctionOutput" $ \v -> ListFunctionOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListFunctionOutputBuilderState = ListFunctionOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Integer,
    total_itemsBuilderState :: Data.Maybe.Maybe Integer,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.FunctionResponse.FunctionResponse)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListFunctionOutputBuilderState
defaultBuilderState = ListFunctionOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

newtype ListFunctionOutputBuilder a = ListFunctionOutputBuilder {
    runListFunctionOutputBuilder :: ListFunctionOutputBuilderState -> (ListFunctionOutputBuilderState, a)
}

instance Data.Functor.Functor ListFunctionOutputBuilder where
    fmap f (ListFunctionOutputBuilder g) =
        ListFunctionOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListFunctionOutputBuilder where
    pure a = ListFunctionOutputBuilder (\s -> (s, a))
    (ListFunctionOutputBuilder f) <*> (ListFunctionOutputBuilder g) = ListFunctionOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListFunctionOutputBuilder where
    (ListFunctionOutputBuilder f) >>= g = ListFunctionOutputBuilder (\s ->
        let (s', a) = f s
            (ListFunctionOutputBuilder h) = g a
        in h s')

setTotalPages :: Data.Maybe.Maybe Integer -> ListFunctionOutputBuilder ()
setTotalPages value =
   ListFunctionOutputBuilder (\s -> (s { total_pagesBuilderState = value }, ()))

setTotalItems :: Data.Maybe.Maybe Integer -> ListFunctionOutputBuilder ()
setTotalItems value =
   ListFunctionOutputBuilder (\s -> (s { total_itemsBuilderState = value }, ()))

setData' :: Data.Maybe.Maybe ([] Io.Superposition.Model.FunctionResponse.FunctionResponse) -> ListFunctionOutputBuilder ()
setData' value =
   ListFunctionOutputBuilder (\s -> (s { data'BuilderState = value }, ()))

build :: ListFunctionOutputBuilder () -> Data.Either.Either Data.Text.Text ListFunctionOutput
build builder = do
    let (st, _) = runListFunctionOutputBuilder builder defaultBuilderState
    total_pages' <- Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListFunctionOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })



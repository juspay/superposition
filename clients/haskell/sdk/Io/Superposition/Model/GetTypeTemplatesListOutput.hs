module Io.Superposition.Model.GetTypeTemplatesListOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    GetTypeTemplatesListOutputBuilder,
    GetTypeTemplatesListOutput,
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
import qualified Io.Superposition.Model.TypeTemplatesResponse

data GetTypeTemplatesListOutput = GetTypeTemplatesListOutput {
    total_pages :: Data.Maybe.Maybe Integer,
    total_items :: Data.Maybe.Maybe Integer,
    data' :: Data.Maybe.Maybe ([] Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetTypeTemplatesListOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON GetTypeTemplatesListOutput where
    parseJSON = Data.Aeson.withObject "GetTypeTemplatesListOutput" $ \v -> GetTypeTemplatesListOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data GetTypeTemplatesListOutputBuilderState = GetTypeTemplatesListOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Integer,
    total_itemsBuilderState :: Data.Maybe.Maybe Integer,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetTypeTemplatesListOutputBuilderState
defaultBuilderState = GetTypeTemplatesListOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

newtype GetTypeTemplatesListOutputBuilder a = GetTypeTemplatesListOutputBuilder {
    runGetTypeTemplatesListOutputBuilder :: GetTypeTemplatesListOutputBuilderState -> (GetTypeTemplatesListOutputBuilderState, a)
}

instance Data.Functor.Functor GetTypeTemplatesListOutputBuilder where
    fmap f (GetTypeTemplatesListOutputBuilder g) =
        GetTypeTemplatesListOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetTypeTemplatesListOutputBuilder where
    pure a = GetTypeTemplatesListOutputBuilder (\s -> (s, a))
    (GetTypeTemplatesListOutputBuilder f) <*> (GetTypeTemplatesListOutputBuilder g) = GetTypeTemplatesListOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetTypeTemplatesListOutputBuilder where
    (GetTypeTemplatesListOutputBuilder f) >>= g = GetTypeTemplatesListOutputBuilder (\s ->
        let (s', a) = f s
            (GetTypeTemplatesListOutputBuilder h) = g a
        in h s')

setTotalPages :: Data.Maybe.Maybe Integer -> GetTypeTemplatesListOutputBuilder ()
setTotalPages value =
   GetTypeTemplatesListOutputBuilder (\s -> (s { total_pagesBuilderState = value }, ()))

setTotalItems :: Data.Maybe.Maybe Integer -> GetTypeTemplatesListOutputBuilder ()
setTotalItems value =
   GetTypeTemplatesListOutputBuilder (\s -> (s { total_itemsBuilderState = value }, ()))

setData' :: Data.Maybe.Maybe ([] Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse) -> GetTypeTemplatesListOutputBuilder ()
setData' value =
   GetTypeTemplatesListOutputBuilder (\s -> (s { data'BuilderState = value }, ()))

build :: GetTypeTemplatesListOutputBuilder () -> Data.Either.Either Data.Text.Text GetTypeTemplatesListOutput
build builder = do
    let (st, _) = runGetTypeTemplatesListOutputBuilder builder defaultBuilderState
    total_pages' <- Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Either.Right (data'BuilderState st)
    Data.Either.Right (GetTypeTemplatesListOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })



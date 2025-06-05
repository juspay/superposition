module Io.Superposition.Model.ListWorkspaceOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    ListWorkspaceOutputBuilder,
    ListWorkspaceOutput,
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
import qualified Data.Int
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.WorkspaceResponse

data ListWorkspaceOutput = ListWorkspaceOutput {
    total_pages :: Data.Int.Int64,
    total_items :: Data.Int.Int64,
    data' :: [] Io.Superposition.Model.WorkspaceResponse.WorkspaceResponse
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListWorkspaceOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON ListWorkspaceOutput where
    parseJSON = Data.Aeson.withObject "ListWorkspaceOutput" $ \v -> ListWorkspaceOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListWorkspaceOutputBuilderState = ListWorkspaceOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    total_itemsBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.WorkspaceResponse.WorkspaceResponse)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListWorkspaceOutputBuilderState
defaultBuilderState = ListWorkspaceOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

newtype ListWorkspaceOutputBuilder a = ListWorkspaceOutputBuilder {
    runListWorkspaceOutputBuilder :: ListWorkspaceOutputBuilderState -> (ListWorkspaceOutputBuilderState, a)
}

instance Data.Functor.Functor ListWorkspaceOutputBuilder where
    fmap f (ListWorkspaceOutputBuilder g) =
        ListWorkspaceOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListWorkspaceOutputBuilder where
    pure a = ListWorkspaceOutputBuilder (\s -> (s, a))
    (ListWorkspaceOutputBuilder f) <*> (ListWorkspaceOutputBuilder g) = ListWorkspaceOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListWorkspaceOutputBuilder where
    (ListWorkspaceOutputBuilder f) >>= g = ListWorkspaceOutputBuilder (\s ->
        let (s', a) = f s
            (ListWorkspaceOutputBuilder h) = g a
        in h s')

setTotalPages :: Data.Int.Int64 -> ListWorkspaceOutputBuilder ()
setTotalPages value =
   ListWorkspaceOutputBuilder (\s -> (s { total_pagesBuilderState = Data.Maybe.Just value }, ()))

setTotalItems :: Data.Int.Int64 -> ListWorkspaceOutputBuilder ()
setTotalItems value =
   ListWorkspaceOutputBuilder (\s -> (s { total_itemsBuilderState = Data.Maybe.Just value }, ()))

setData' :: [] Io.Superposition.Model.WorkspaceResponse.WorkspaceResponse -> ListWorkspaceOutputBuilder ()
setData' value =
   ListWorkspaceOutputBuilder (\s -> (s { data'BuilderState = Data.Maybe.Just value }, ()))

build :: ListWorkspaceOutputBuilder () -> Data.Either.Either Data.Text.Text ListWorkspaceOutput
build builder = do
    let (st, _) = runListWorkspaceOutputBuilder builder defaultBuilderState
    total_pages' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWorkspaceOutput.ListWorkspaceOutput.total_pages is a required property.") Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWorkspaceOutput.ListWorkspaceOutput.total_items is a required property.") Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWorkspaceOutput.ListWorkspaceOutput.data' is a required property.") Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListWorkspaceOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })



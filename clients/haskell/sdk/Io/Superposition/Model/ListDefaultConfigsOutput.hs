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
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.DefaultConfigFull
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data ListDefaultConfigsOutput = ListDefaultConfigsOutput {
    total_pages :: Data.Maybe.Maybe Data.Int.Int32,
    total_items :: Data.Maybe.Maybe Data.Int.Int32,
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
    

instance Io.Superposition.Utility.SerializeBody ListDefaultConfigsOutput

instance Data.Aeson.FromJSON ListDefaultConfigsOutput where
    parseJSON = Data.Aeson.withObject "ListDefaultConfigsOutput" $ \v -> ListDefaultConfigsOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListDefaultConfigsOutputBuilderState = ListDefaultConfigsOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    total_itemsBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
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

type ListDefaultConfigsOutputBuilder = Control.Monad.State.Strict.State ListDefaultConfigsOutputBuilderState

setTotalPages :: Data.Maybe.Maybe Data.Int.Int32 -> ListDefaultConfigsOutputBuilder ()
setTotalPages value =
   Control.Monad.State.Strict.modify (\s -> (s { total_pagesBuilderState = value }))

setTotalItems :: Data.Maybe.Maybe Data.Int.Int32 -> ListDefaultConfigsOutputBuilder ()
setTotalItems value =
   Control.Monad.State.Strict.modify (\s -> (s { total_itemsBuilderState = value }))

setData' :: Data.Maybe.Maybe ([] Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull) -> ListDefaultConfigsOutputBuilder ()
setData' value =
   Control.Monad.State.Strict.modify (\s -> (s { data'BuilderState = value }))

build :: ListDefaultConfigsOutputBuilder () -> Data.Either.Either Data.Text.Text ListDefaultConfigsOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    total_pages' <- Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListDefaultConfigsOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })


instance Io.Superposition.Utility.FromResponseParser ListDefaultConfigsOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "data"
        var1 <- Io.Superposition.Utility.deSerField "total_pages"
        var2 <- Io.Superposition.Utility.deSerField "total_items"
        pure $ ListDefaultConfigsOutput {
            total_pages = var1,
            total_items = var2,
            data' = var0
        }


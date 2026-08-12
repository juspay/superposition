module Io.Superposition.Model.ListJobsOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    ListJobsOutputBuilder,
    ListJobsOutput,
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
import qualified Io.Superposition.Model.JobDetailResponse
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data ListJobsOutput = ListJobsOutput {
    total_pages :: Data.Int.Int32,
    total_items :: Data.Int.Int32,
    data' :: [] Io.Superposition.Model.JobDetailResponse.JobDetailResponse
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListJobsOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListJobsOutput

instance Data.Aeson.FromJSON ListJobsOutput where
    parseJSON = Data.Aeson.withObject "ListJobsOutput" $ \v -> ListJobsOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListJobsOutputBuilderState = ListJobsOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    total_itemsBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.JobDetailResponse.JobDetailResponse)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListJobsOutputBuilderState
defaultBuilderState = ListJobsOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

type ListJobsOutputBuilder = Control.Monad.State.Strict.State ListJobsOutputBuilderState

setTotalPages :: Data.Int.Int32 -> ListJobsOutputBuilder ()
setTotalPages value =
   Control.Monad.State.Strict.modify (\s -> (s { total_pagesBuilderState = Data.Maybe.Just value }))

setTotalItems :: Data.Int.Int32 -> ListJobsOutputBuilder ()
setTotalItems value =
   Control.Monad.State.Strict.modify (\s -> (s { total_itemsBuilderState = Data.Maybe.Just value }))

setData' :: [] Io.Superposition.Model.JobDetailResponse.JobDetailResponse -> ListJobsOutputBuilder ()
setData' value =
   Control.Monad.State.Strict.modify (\s -> (s { data'BuilderState = Data.Maybe.Just value }))

build :: ListJobsOutputBuilder () -> Data.Either.Either Data.Text.Text ListJobsOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    total_pages' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListJobsOutput.ListJobsOutput.total_pages is a required property.") Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListJobsOutput.ListJobsOutput.total_items is a required property.") Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListJobsOutput.ListJobsOutput.data' is a required property.") Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListJobsOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })


instance Io.Superposition.Utility.FromResponseParser ListJobsOutput where
    expectedStatus = (Network.HTTP.Types.mkStatus 200 "")
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "data"
        var1 <- Io.Superposition.Utility.deSerField "total_pages"
        var2 <- Io.Superposition.Utility.deSerField "total_items"
        pure $ ListJobsOutput {
            total_pages = var1,
            total_items = var2,
            data' = var0
        }


module Io.Superposition.Model.ListExperimentGroupsOutput (
    setTotalPages,
    setTotalItems,
    setData',
    setLastModified,
    build,
    ListExperimentGroupsOutputBuilder,
    ListExperimentGroupsOutput,
    total_pages,
    total_items,
    data',
    last_modified
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
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ExperimentGroupResponse
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data ListExperimentGroupsOutput = ListExperimentGroupsOutput {
    total_pages :: Data.Int.Int32,
    total_items :: Data.Int.Int32,
    data' :: [] Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse,
    last_modified :: Data.Time.UTCTime
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListExperimentGroupsOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a,
        "last_modified" Data.Aeson..= last_modified a
        ]
    

instance Io.Superposition.Utility.SerializeBody ListExperimentGroupsOutput

instance Data.Aeson.FromJSON ListExperimentGroupsOutput where
    parseJSON = Data.Aeson.withObject "ListExperimentGroupsOutput" $ \v -> ListExperimentGroupsOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified")
    



data ListExperimentGroupsOutputBuilderState = ListExperimentGroupsOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    total_itemsBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse),
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListExperimentGroupsOutputBuilderState
defaultBuilderState = ListExperimentGroupsOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing,
    last_modifiedBuilderState = Data.Maybe.Nothing
}

type ListExperimentGroupsOutputBuilder = Control.Monad.State.Strict.State ListExperimentGroupsOutputBuilderState

setTotalPages :: Data.Int.Int32 -> ListExperimentGroupsOutputBuilder ()
setTotalPages value =
   Control.Monad.State.Strict.modify (\s -> (s { total_pagesBuilderState = Data.Maybe.Just value }))

setTotalItems :: Data.Int.Int32 -> ListExperimentGroupsOutputBuilder ()
setTotalItems value =
   Control.Monad.State.Strict.modify (\s -> (s { total_itemsBuilderState = Data.Maybe.Just value }))

setData' :: [] Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse -> ListExperimentGroupsOutputBuilder ()
setData' value =
   Control.Monad.State.Strict.modify (\s -> (s { data'BuilderState = Data.Maybe.Just value }))

setLastModified :: Data.Time.UTCTime -> ListExperimentGroupsOutputBuilder ()
setLastModified value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modifiedBuilderState = Data.Maybe.Just value }))

build :: ListExperimentGroupsOutputBuilder () -> Data.Either.Either Data.Text.Text ListExperimentGroupsOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    total_pages' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentGroupsOutput.ListExperimentGroupsOutput.total_pages is a required property.") Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentGroupsOutput.ListExperimentGroupsOutput.total_items is a required property.") Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentGroupsOutput.ListExperimentGroupsOutput.data' is a required property.") Data.Either.Right (data'BuilderState st)
    last_modified' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentGroupsOutput.ListExperimentGroupsOutput.last_modified is a required property.") Data.Either.Right (last_modifiedBuilderState st)
    Data.Either.Right (ListExperimentGroupsOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data'',
        last_modified = last_modified'
    })


instance Io.Superposition.Utility.FromResponseParser ListExperimentGroupsOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        var0 <- Io.Superposition.Utility.deSerHeader "last-modified"
        var1 <- Io.Superposition.Utility.deSerField "data"
        var2 <- Io.Superposition.Utility.deSerField "total_pages"
        var3 <- Io.Superposition.Utility.deSerField "total_items"
        pure $ ListExperimentGroupsOutput {
            total_pages = var2,
            total_items = var3,
            data' = var1,
            last_modified = var0
        }


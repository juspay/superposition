module Io.Superposition.Model.ListExperimentOutput (
    setTotalPages,
    setTotalItems,
    setData',
<<<<<<< HEAD
<<<<<<< HEAD
    setLastModified,
=======
    setLastModifiedAt,
>>>>>>> 6e8749e1 (Test)
=======
    setLastModified,
>>>>>>> 82479b8f (fix: more fixes)
    build,
    ListExperimentOutputBuilder,
    ListExperimentOutput,
    total_pages,
    total_items,
    data',
<<<<<<< HEAD
<<<<<<< HEAD
    last_modified
=======
    last_modified_at
>>>>>>> 6e8749e1 (Test)
=======
    last_modified
>>>>>>> 82479b8f (fix: more fixes)
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
import qualified Io.Superposition.Model.ExperimentResponse
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data ListExperimentOutput = ListExperimentOutput {
    total_pages :: Data.Int.Int32,
    total_items :: Data.Int.Int32,
    data' :: [] Io.Superposition.Model.ExperimentResponse.ExperimentResponse,
<<<<<<< HEAD
<<<<<<< HEAD
    last_modified :: Data.Time.UTCTime
=======
    last_modified_at :: Data.Time.UTCTime
>>>>>>> 6e8749e1 (Test)
=======
    last_modified :: Data.Time.UTCTime
>>>>>>> 82479b8f (fix: more fixes)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListExperimentOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a,
<<<<<<< HEAD
<<<<<<< HEAD
        "last_modified" Data.Aeson..= last_modified a
=======
        "last_modified_at" Data.Aeson..= last_modified_at a
>>>>>>> 6e8749e1 (Test)
=======
        "last_modified" Data.Aeson..= last_modified a
>>>>>>> 82479b8f (fix: more fixes)
        ]
    

instance Io.Superposition.Utility.SerializeBody ListExperimentOutput

instance Data.Aeson.FromJSON ListExperimentOutput where
    parseJSON = Data.Aeson.withObject "ListExperimentOutput" $ \v -> ListExperimentOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
<<<<<<< HEAD
<<<<<<< HEAD
        Control.Applicative.<*> (v Data.Aeson..: "last_modified")
=======
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
>>>>>>> 6e8749e1 (Test)
=======
        Control.Applicative.<*> (v Data.Aeson..: "last_modified")
>>>>>>> 82479b8f (fix: more fixes)
    



data ListExperimentOutputBuilderState = ListExperimentOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    total_itemsBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ExperimentResponse.ExperimentResponse),
<<<<<<< HEAD
<<<<<<< HEAD
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
=======
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
>>>>>>> 6e8749e1 (Test)
=======
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
>>>>>>> 82479b8f (fix: more fixes)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListExperimentOutputBuilderState
defaultBuilderState = ListExperimentOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing,
<<<<<<< HEAD
<<<<<<< HEAD
    last_modifiedBuilderState = Data.Maybe.Nothing
=======
    last_modified_atBuilderState = Data.Maybe.Nothing
>>>>>>> 6e8749e1 (Test)
=======
    last_modifiedBuilderState = Data.Maybe.Nothing
>>>>>>> 82479b8f (fix: more fixes)
}

type ListExperimentOutputBuilder = Control.Monad.State.Strict.State ListExperimentOutputBuilderState

setTotalPages :: Data.Int.Int32 -> ListExperimentOutputBuilder ()
setTotalPages value =
   Control.Monad.State.Strict.modify (\s -> (s { total_pagesBuilderState = Data.Maybe.Just value }))

setTotalItems :: Data.Int.Int32 -> ListExperimentOutputBuilder ()
setTotalItems value =
   Control.Monad.State.Strict.modify (\s -> (s { total_itemsBuilderState = Data.Maybe.Just value }))

setData' :: [] Io.Superposition.Model.ExperimentResponse.ExperimentResponse -> ListExperimentOutputBuilder ()
setData' value =
   Control.Monad.State.Strict.modify (\s -> (s { data'BuilderState = Data.Maybe.Just value }))

<<<<<<< HEAD
<<<<<<< HEAD
setLastModified :: Data.Time.UTCTime -> ListExperimentOutputBuilder ()
setLastModified value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modifiedBuilderState = Data.Maybe.Just value }))
=======
setLastModifiedAt :: Data.Time.UTCTime -> ListExperimentOutputBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))
>>>>>>> 6e8749e1 (Test)
=======
setLastModified :: Data.Time.UTCTime -> ListExperimentOutputBuilder ()
setLastModified value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modifiedBuilderState = Data.Maybe.Just value }))
>>>>>>> 82479b8f (fix: more fixes)

build :: ListExperimentOutputBuilder () -> Data.Either.Either Data.Text.Text ListExperimentOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    total_pages' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentOutput.ListExperimentOutput.total_pages is a required property.") Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentOutput.ListExperimentOutput.total_items is a required property.") Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentOutput.ListExperimentOutput.data' is a required property.") Data.Either.Right (data'BuilderState st)
<<<<<<< HEAD
<<<<<<< HEAD
    last_modified' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentOutput.ListExperimentOutput.last_modified is a required property.") Data.Either.Right (last_modifiedBuilderState st)
=======
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentOutput.ListExperimentOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
>>>>>>> 6e8749e1 (Test)
=======
    last_modified' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListExperimentOutput.ListExperimentOutput.last_modified is a required property.") Data.Either.Right (last_modifiedBuilderState st)
>>>>>>> 82479b8f (fix: more fixes)
    Data.Either.Right (ListExperimentOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data'',
<<<<<<< HEAD
<<<<<<< HEAD
        last_modified = last_modified'
=======
        last_modified_at = last_modified_at'
>>>>>>> 6e8749e1 (Test)
=======
        last_modified = last_modified'
>>>>>>> 82479b8f (fix: more fixes)
    })


instance Io.Superposition.Utility.FromResponseParser ListExperimentOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        var0 <- Io.Superposition.Utility.deSerHeader "last-modified"
        var1 <- Io.Superposition.Utility.deSerField "data"
        var2 <- Io.Superposition.Utility.deSerField "total_pages"
        var3 <- Io.Superposition.Utility.deSerField "total_items"
        pure $ ListExperimentOutput {
            total_pages = var2,
            total_items = var3,
            data' = var1,
<<<<<<< HEAD
<<<<<<< HEAD
            last_modified = var0
=======
            last_modified_at = var0
>>>>>>> 6e8749e1 (Test)
=======
            last_modified = var0
>>>>>>> 82479b8f (fix: more fixes)
        }


module Io.Superposition.Model.ImportErrorItem (
    setId',
    setError,
    build,
    ImportErrorItemBuilder,
    ImportErrorItem,
    id',
    error
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

data ImportErrorItem = ImportErrorItem {
    id' :: Data.Text.Text,
    error :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ImportErrorItem where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "error" Data.Aeson..= error a
        ]
    

instance Io.Superposition.Utility.SerializeBody ImportErrorItem

instance Data.Aeson.FromJSON ImportErrorItem where
    parseJSON = Data.Aeson.withObject "ImportErrorItem" $ \v -> ImportErrorItem
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "error")
    



data ImportErrorItemBuilderState = ImportErrorItemBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    errorBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ImportErrorItemBuilderState
defaultBuilderState = ImportErrorItemBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    errorBuilderState = Data.Maybe.Nothing
}

type ImportErrorItemBuilder = Control.Monad.State.Strict.State ImportErrorItemBuilderState

setId' :: Data.Text.Text -> ImportErrorItemBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setError :: Data.Text.Text -> ImportErrorItemBuilder ()
setError value =
   Control.Monad.State.Strict.modify (\s -> (s { errorBuilderState = Data.Maybe.Just value }))

build :: ImportErrorItemBuilder () -> Data.Either.Either Data.Text.Text ImportErrorItem
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportErrorItem.ImportErrorItem.id' is a required property.") Data.Either.Right (id'BuilderState st)
    error' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportErrorItem.ImportErrorItem.error is a required property.") Data.Either.Right (errorBuilderState st)
    Data.Either.Right (ImportErrorItem { 
        id' = id'',
        error = error'
    })



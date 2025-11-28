module Io.Superposition.Model.BulkOperationOut (
    setOutput,
    build,
    BulkOperationOutBuilder,
    BulkOperationOut,
    output
) where
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ContextActionOut
import qualified Io.Superposition.Utility

data BulkOperationOut = BulkOperationOut {
    output :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextActionOut.ContextActionOut)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON BulkOperationOut where
    toJSON a = Data.Aeson.object [
        "output" Data.Aeson..= output a
        ]
    

instance Io.Superposition.Utility.SerializeBody BulkOperationOut

instance Data.Aeson.FromJSON BulkOperationOut where
    parseJSON = Data.Aeson.withObject "BulkOperationOut" $ \v -> BulkOperationOut
        Data.Functor.<$> (v Data.Aeson..: "output")
    



data BulkOperationOutBuilderState = BulkOperationOutBuilderState {
    outputBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextActionOut.ContextActionOut)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: BulkOperationOutBuilderState
defaultBuilderState = BulkOperationOutBuilderState {
    outputBuilderState = Data.Maybe.Nothing
}

type BulkOperationOutBuilder = Control.Monad.State.Strict.State BulkOperationOutBuilderState

setOutput :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextActionOut.ContextActionOut) -> BulkOperationOutBuilder ()
setOutput value =
   Control.Monad.State.Strict.modify (\s -> (s { outputBuilderState = value }))

build :: BulkOperationOutBuilder () -> Data.Either.Either Data.Text.Text BulkOperationOut
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    output' <- Data.Either.Right (outputBuilderState st)
    Data.Either.Right (BulkOperationOut { 
        output = output'
    })



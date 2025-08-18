module Io.Superposition.Model.BulkOperationOutput (
    setBulkOperationOutput,
    build,
    BulkOperationOutputBuilder,
    BulkOperationOutput,
    bulk_operation_output
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
import qualified Io.Superposition.Model.BulkOperationOut
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data BulkOperationOutput = BulkOperationOutput {
    bulk_operation_output :: Data.Maybe.Maybe Io.Superposition.Model.BulkOperationOut.BulkOperationOut
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON BulkOperationOutput where
    toJSON a = Data.Aeson.object [
        "bulk_operation_output" Data.Aeson..= bulk_operation_output a
        ]
    

instance Io.Superposition.Utility.SerializeBody BulkOperationOutput

instance Data.Aeson.FromJSON BulkOperationOutput where
    parseJSON = Data.Aeson.withObject "BulkOperationOutput" $ \v -> BulkOperationOutput
        Data.Functor.<$> (v Data.Aeson..: "bulk_operation_output")
    



data BulkOperationOutputBuilderState = BulkOperationOutputBuilderState {
    bulk_operation_outputBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.BulkOperationOut.BulkOperationOut
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: BulkOperationOutputBuilderState
defaultBuilderState = BulkOperationOutputBuilderState {
    bulk_operation_outputBuilderState = Data.Maybe.Nothing
}

type BulkOperationOutputBuilder = Control.Monad.State.Strict.State BulkOperationOutputBuilderState

setBulkOperationOutput :: Data.Maybe.Maybe Io.Superposition.Model.BulkOperationOut.BulkOperationOut -> BulkOperationOutputBuilder ()
setBulkOperationOutput value =
   Control.Monad.State.Strict.modify (\s -> (s { bulk_operation_outputBuilderState = value }))

build :: BulkOperationOutputBuilder () -> Data.Either.Either Data.Text.Text BulkOperationOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    bulk_operation_output' <- Data.Either.Right (bulk_operation_outputBuilderState st)
    Data.Either.Right (BulkOperationOutput { 
        bulk_operation_output = bulk_operation_output'
    })


instance Io.Superposition.Utility.FromResponseParser BulkOperationOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerBody
        pure $ BulkOperationOutput {
            bulk_operation_output = var0
        }


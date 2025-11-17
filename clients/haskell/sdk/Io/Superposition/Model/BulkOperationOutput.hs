module Io.Superposition.Model.BulkOperationOutput (
    setOutput,
    build,
    BulkOperationOutputBuilder,
    BulkOperationOutput,
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
import qualified Network.HTTP.Types

data BulkOperationOutput = BulkOperationOutput {
    output :: [] Io.Superposition.Model.ContextActionOut.ContextActionOut
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON BulkOperationOutput where
    toJSON a = Data.Aeson.object [
        "output" Data.Aeson..= output a
        ]
    

instance Io.Superposition.Utility.SerializeBody BulkOperationOutput

instance Data.Aeson.FromJSON BulkOperationOutput where
    parseJSON = Data.Aeson.withObject "BulkOperationOutput" $ \v -> BulkOperationOutput
        Data.Functor.<$> (v Data.Aeson..: "output")
    



data BulkOperationOutputBuilderState = BulkOperationOutputBuilderState {
    outputBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextActionOut.ContextActionOut)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: BulkOperationOutputBuilderState
defaultBuilderState = BulkOperationOutputBuilderState {
    outputBuilderState = Data.Maybe.Nothing
}

type BulkOperationOutputBuilder = Control.Monad.State.Strict.State BulkOperationOutputBuilderState

setOutput :: [] Io.Superposition.Model.ContextActionOut.ContextActionOut -> BulkOperationOutputBuilder ()
setOutput value =
   Control.Monad.State.Strict.modify (\s -> (s { outputBuilderState = Data.Maybe.Just value }))

build :: BulkOperationOutputBuilder () -> Data.Either.Either Data.Text.Text BulkOperationOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    output' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.BulkOperationOutput.BulkOperationOutput.output is a required property.") Data.Either.Right (outputBuilderState st)
    Data.Either.Right (BulkOperationOutput { 
        output = output'
    })


instance Io.Superposition.Utility.FromResponseParser BulkOperationOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "output"
        pure $ BulkOperationOutput {
            output = var0
        }


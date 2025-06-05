module Io.Superposition.Model.BulkOperationOutput (
    setBulkOperationOutput,
    build,
    BulkOperationOutputBuilder,
    BulkOperationOutput,
    bulk_operation_output
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
import qualified Io.Superposition.Model.BulkOperationOut

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

newtype BulkOperationOutputBuilder a = BulkOperationOutputBuilder {
    runBulkOperationOutputBuilder :: BulkOperationOutputBuilderState -> (BulkOperationOutputBuilderState, a)
}

instance Data.Functor.Functor BulkOperationOutputBuilder where
    fmap f (BulkOperationOutputBuilder g) =
        BulkOperationOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative BulkOperationOutputBuilder where
    pure a = BulkOperationOutputBuilder (\s -> (s, a))
    (BulkOperationOutputBuilder f) <*> (BulkOperationOutputBuilder g) = BulkOperationOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad BulkOperationOutputBuilder where
    (BulkOperationOutputBuilder f) >>= g = BulkOperationOutputBuilder (\s ->
        let (s', a) = f s
            (BulkOperationOutputBuilder h) = g a
        in h s')

setBulkOperationOutput :: Data.Maybe.Maybe Io.Superposition.Model.BulkOperationOut.BulkOperationOut -> BulkOperationOutputBuilder ()
setBulkOperationOutput value =
   BulkOperationOutputBuilder (\s -> (s { bulk_operation_outputBuilderState = value }, ()))

build :: BulkOperationOutputBuilder () -> Data.Either.Either Data.Text.Text BulkOperationOutput
build builder = do
    let (st, _) = runBulkOperationOutputBuilder builder defaultBuilderState
    bulk_operation_output' <- Data.Either.Right (bulk_operation_outputBuilderState st)
    Data.Either.Right (BulkOperationOutput { 
        bulk_operation_output = bulk_operation_output'
    })



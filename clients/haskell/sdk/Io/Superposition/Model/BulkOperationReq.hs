module Io.Superposition.Model.BulkOperationReq (
    setOperations,
    build,
    BulkOperationReqBuilder,
    BulkOperationReq,
    operations
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
import qualified Io.Superposition.Model.ContextAction
import qualified Io.Superposition.Utility

data BulkOperationReq = BulkOperationReq {
    operations :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextAction.ContextAction)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON BulkOperationReq where
    toJSON a = Data.Aeson.object [
        "operations" Data.Aeson..= operations a
        ]
    

instance Io.Superposition.Utility.SerializeBody BulkOperationReq

instance Data.Aeson.FromJSON BulkOperationReq where
    parseJSON = Data.Aeson.withObject "BulkOperationReq" $ \v -> BulkOperationReq
        Data.Functor.<$> (v Data.Aeson..: "operations")
    



data BulkOperationReqBuilderState = BulkOperationReqBuilderState {
    operationsBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextAction.ContextAction)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: BulkOperationReqBuilderState
defaultBuilderState = BulkOperationReqBuilderState {
    operationsBuilderState = Data.Maybe.Nothing
}

type BulkOperationReqBuilder = Control.Monad.State.Strict.State BulkOperationReqBuilderState

setOperations :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextAction.ContextAction) -> BulkOperationReqBuilder ()
setOperations value =
   Control.Monad.State.Strict.modify (\s -> (s { operationsBuilderState = value }))

build :: BulkOperationReqBuilder () -> Data.Either.Either Data.Text.Text BulkOperationReq
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    operations' <- Data.Either.Right (operationsBuilderState st)
    Data.Either.Right (BulkOperationReq { 
        operations = operations'
    })



module Io.Superposition.Model.BulkOperationReq (
    setOperations,
    build,
    BulkOperationReqBuilder,
    BulkOperationReq,
    operations
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
import qualified Io.Superposition.Model.ContextAction

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

newtype BulkOperationReqBuilder a = BulkOperationReqBuilder {
    runBulkOperationReqBuilder :: BulkOperationReqBuilderState -> (BulkOperationReqBuilderState, a)
}

instance Data.Functor.Functor BulkOperationReqBuilder where
    fmap f (BulkOperationReqBuilder g) =
        BulkOperationReqBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative BulkOperationReqBuilder where
    pure a = BulkOperationReqBuilder (\s -> (s, a))
    (BulkOperationReqBuilder f) <*> (BulkOperationReqBuilder g) = BulkOperationReqBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad BulkOperationReqBuilder where
    (BulkOperationReqBuilder f) >>= g = BulkOperationReqBuilder (\s ->
        let (s', a) = f s
            (BulkOperationReqBuilder h) = g a
        in h s')

setOperations :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextAction.ContextAction) -> BulkOperationReqBuilder ()
setOperations value =
   BulkOperationReqBuilder (\s -> (s { operationsBuilderState = value }, ()))

build :: BulkOperationReqBuilder () -> Data.Either.Either Data.Text.Text BulkOperationReq
build builder = do
    let (st, _) = runBulkOperationReqBuilder builder defaultBuilderState
    operations' <- Data.Either.Right (operationsBuilderState st)
    Data.Either.Right (BulkOperationReq { 
        operations = operations'
    })



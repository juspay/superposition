module Io.Superposition.Model.BulkOperationOut (
    setOutput,
    build,
    BulkOperationOutBuilder,
    BulkOperationOut,
    output
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
import qualified Io.Superposition.Model.ContextActionOut

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

newtype BulkOperationOutBuilder a = BulkOperationOutBuilder {
    runBulkOperationOutBuilder :: BulkOperationOutBuilderState -> (BulkOperationOutBuilderState, a)
}

instance Data.Functor.Functor BulkOperationOutBuilder where
    fmap f (BulkOperationOutBuilder g) =
        BulkOperationOutBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative BulkOperationOutBuilder where
    pure a = BulkOperationOutBuilder (\s -> (s, a))
    (BulkOperationOutBuilder f) <*> (BulkOperationOutBuilder g) = BulkOperationOutBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad BulkOperationOutBuilder where
    (BulkOperationOutBuilder f) >>= g = BulkOperationOutBuilder (\s ->
        let (s', a) = f s
            (BulkOperationOutBuilder h) = g a
        in h s')

setOutput :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextActionOut.ContextActionOut) -> BulkOperationOutBuilder ()
setOutput value =
   BulkOperationOutBuilder (\s -> (s { outputBuilderState = value }, ()))

build :: BulkOperationOutBuilder () -> Data.Either.Either Data.Text.Text BulkOperationOut
build builder = do
    let (st, _) = runBulkOperationOutBuilder builder defaultBuilderState
    output' <- Data.Either.Right (outputBuilderState st)
    Data.Either.Right (BulkOperationOut { 
        output = output'
    })



module Io.Superposition.Model.WeightRecomputeOutput (
    setData',
    build,
    WeightRecomputeOutputBuilder,
    WeightRecomputeOutput,
    data'
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
import qualified Io.Superposition.Model.WeightRecomputeResponse

data WeightRecomputeOutput = WeightRecomputeOutput {
    data' :: Data.Maybe.Maybe ([] Io.Superposition.Model.WeightRecomputeResponse.WeightRecomputeResponse)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WeightRecomputeOutput where
    toJSON a = Data.Aeson.object [
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON WeightRecomputeOutput where
    parseJSON = Data.Aeson.withObject "WeightRecomputeOutput" $ \v -> WeightRecomputeOutput
        Data.Functor.<$> (v Data.Aeson..: "data")
    



data WeightRecomputeOutputBuilderState = WeightRecomputeOutputBuilderState {
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.WeightRecomputeResponse.WeightRecomputeResponse)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WeightRecomputeOutputBuilderState
defaultBuilderState = WeightRecomputeOutputBuilderState {
    data'BuilderState = Data.Maybe.Nothing
}

newtype WeightRecomputeOutputBuilder a = WeightRecomputeOutputBuilder {
    runWeightRecomputeOutputBuilder :: WeightRecomputeOutputBuilderState -> (WeightRecomputeOutputBuilderState, a)
}

instance Data.Functor.Functor WeightRecomputeOutputBuilder where
    fmap f (WeightRecomputeOutputBuilder g) =
        WeightRecomputeOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative WeightRecomputeOutputBuilder where
    pure a = WeightRecomputeOutputBuilder (\s -> (s, a))
    (WeightRecomputeOutputBuilder f) <*> (WeightRecomputeOutputBuilder g) = WeightRecomputeOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad WeightRecomputeOutputBuilder where
    (WeightRecomputeOutputBuilder f) >>= g = WeightRecomputeOutputBuilder (\s ->
        let (s', a) = f s
            (WeightRecomputeOutputBuilder h) = g a
        in h s')

setData' :: Data.Maybe.Maybe ([] Io.Superposition.Model.WeightRecomputeResponse.WeightRecomputeResponse) -> WeightRecomputeOutputBuilder ()
setData' value =
   WeightRecomputeOutputBuilder (\s -> (s { data'BuilderState = value }, ()))

build :: WeightRecomputeOutputBuilder () -> Data.Either.Either Data.Text.Text WeightRecomputeOutput
build builder = do
    let (st, _) = runWeightRecomputeOutputBuilder builder defaultBuilderState
    data'' <- Data.Either.Right (data'BuilderState st)
    Data.Either.Right (WeightRecomputeOutput { 
        data' = data''
    })



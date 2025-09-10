module Io.Superposition.Model.WeightRecomputeOutput (
    setData',
    build,
    WeightRecomputeOutputBuilder,
    WeightRecomputeOutput,
    data'
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
import qualified Io.Superposition.Model.WeightRecomputeResponse
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

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
    

instance Io.Superposition.Utility.SerializeBody WeightRecomputeOutput

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

type WeightRecomputeOutputBuilder = Control.Monad.State.Strict.State WeightRecomputeOutputBuilderState

setData' :: Data.Maybe.Maybe ([] Io.Superposition.Model.WeightRecomputeResponse.WeightRecomputeResponse) -> WeightRecomputeOutputBuilder ()
setData' value =
   Control.Monad.State.Strict.modify (\s -> (s { data'BuilderState = value }))

build :: WeightRecomputeOutputBuilder () -> Data.Either.Either Data.Text.Text WeightRecomputeOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    data'' <- Data.Either.Right (data'BuilderState st)
    Data.Either.Right (WeightRecomputeOutput { 
        data' = data''
    })


instance Io.Superposition.Utility.FromResponseParser WeightRecomputeOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "data"
        pure $ WeightRecomputeOutput {
            data' = var0
        }


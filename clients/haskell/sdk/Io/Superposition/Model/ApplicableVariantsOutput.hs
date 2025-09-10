module Io.Superposition.Model.ApplicableVariantsOutput (
    setData',
    build,
    ApplicableVariantsOutputBuilder,
    ApplicableVariantsOutput,
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
import qualified Io.Superposition.Model.Variant
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data ApplicableVariantsOutput = ApplicableVariantsOutput {
    data' :: [] Io.Superposition.Model.Variant.Variant
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ApplicableVariantsOutput where
    toJSON a = Data.Aeson.object [
        "data" Data.Aeson..= data' a
        ]
    

instance Io.Superposition.Utility.SerializeBody ApplicableVariantsOutput

instance Data.Aeson.FromJSON ApplicableVariantsOutput where
    parseJSON = Data.Aeson.withObject "ApplicableVariantsOutput" $ \v -> ApplicableVariantsOutput
        Data.Functor.<$> (v Data.Aeson..: "data")
    



data ApplicableVariantsOutputBuilderState = ApplicableVariantsOutputBuilderState {
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.Variant.Variant)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ApplicableVariantsOutputBuilderState
defaultBuilderState = ApplicableVariantsOutputBuilderState {
    data'BuilderState = Data.Maybe.Nothing
}

type ApplicableVariantsOutputBuilder = Control.Monad.State.Strict.State ApplicableVariantsOutputBuilderState

setData' :: [] Io.Superposition.Model.Variant.Variant -> ApplicableVariantsOutputBuilder ()
setData' value =
   Control.Monad.State.Strict.modify (\s -> (s { data'BuilderState = Data.Maybe.Just value }))

build :: ApplicableVariantsOutputBuilder () -> Data.Either.Either Data.Text.Text ApplicableVariantsOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    data'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ApplicableVariantsOutput.ApplicableVariantsOutput.data' is a required property.") Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ApplicableVariantsOutput { 
        data' = data''
    })


instance Io.Superposition.Utility.FromResponseParser ApplicableVariantsOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "data"
        pure $ ApplicableVariantsOutput {
            data' = var0
        }


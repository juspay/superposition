module Io.Superposition.Model.ContextValidationFunctionRequest (
    setEnvironment,
    build,
    ContextValidationFunctionRequestBuilder,
    ContextValidationFunctionRequest,
    environment
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
import qualified Io.Superposition.Utility

data ContextValidationFunctionRequest = ContextValidationFunctionRequest {
    environment :: Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ContextValidationFunctionRequest where
    toJSON a = Data.Aeson.object [
        "environment" Data.Aeson..= environment a
        ]
    

instance Io.Superposition.Utility.SerializeBody ContextValidationFunctionRequest

instance Data.Aeson.FromJSON ContextValidationFunctionRequest where
    parseJSON = Data.Aeson.withObject "ContextValidationFunctionRequest" $ \v -> ContextValidationFunctionRequest
        Data.Functor.<$> (v Data.Aeson..: "environment")
    



data ContextValidationFunctionRequestBuilderState = ContextValidationFunctionRequestBuilderState {
    environmentBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ContextValidationFunctionRequestBuilderState
defaultBuilderState = ContextValidationFunctionRequestBuilderState {
    environmentBuilderState = Data.Maybe.Nothing
}

type ContextValidationFunctionRequestBuilder = Control.Monad.State.Strict.State ContextValidationFunctionRequestBuilderState

setEnvironment :: Data.Aeson.Value -> ContextValidationFunctionRequestBuilder ()
setEnvironment value =
   Control.Monad.State.Strict.modify (\s -> (s { environmentBuilderState = Data.Maybe.Just value }))

build :: ContextValidationFunctionRequestBuilder () -> Data.Either.Either Data.Text.Text ContextValidationFunctionRequest
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    environment' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ContextValidationFunctionRequest.ContextValidationFunctionRequest.environment is a required property.") Data.Either.Right (environmentBuilderState st)
    Data.Either.Right (ContextValidationFunctionRequest { 
        environment = environment'
    })



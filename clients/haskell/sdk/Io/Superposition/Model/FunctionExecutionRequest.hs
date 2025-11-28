module Io.Superposition.Model.FunctionExecutionRequest (
    FunctionExecutionRequest(..)
) where
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Functor
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ContextValidationFunctionRequest
import qualified Io.Superposition.Model.ValueComputeFunctionRequest
import qualified Io.Superposition.Model.ValueValidationFunctionRequest
import qualified Io.Superposition.Utility

-- Union implementation for FunctionExecutionRequest
data FunctionExecutionRequest =
    Valuevalidationfunctionrequest (Io.Superposition.Model.ValueValidationFunctionRequest.ValueValidationFunctionRequest)
    | Valuecomputefunctionrequest (Io.Superposition.Model.ValueComputeFunctionRequest.ValueComputeFunctionRequest)
    | Contextvalidationfunctionrequest (Io.Superposition.Model.ContextValidationFunctionRequest.ContextValidationFunctionRequest)
    deriving (
    GHC.Generics.Generic,
    GHC.Show.Show,
    Data.Eq.Eq
    )

instance Data.Aeson.ToJSON FunctionExecutionRequest where
    toJSON (Valuevalidationfunctionrequest a) = Data.Aeson.object [ "ValueValidationFunctionRequest" Data.Aeson..= a ]
    toJSON (Valuecomputefunctionrequest a) = Data.Aeson.object [ "ValueComputeFunctionRequest" Data.Aeson..= a ]
    toJSON (Contextvalidationfunctionrequest a) = Data.Aeson.object [ "ContextValidationFunctionRequest" Data.Aeson..= a ]

instance Io.Superposition.Utility.SerializeBody FunctionExecutionRequest
instance Data.Aeson.FromJSON FunctionExecutionRequest where
    parseJSON = Data.Aeson.withObject "FunctionExecutionRequest" $ \v ->
        (Valuevalidationfunctionrequest Data.Functor.<$> v Data.Aeson..: "ValueValidationFunctionRequest") Control.Applicative.<|>
        (Valuecomputefunctionrequest Data.Functor.<$> v Data.Aeson..: "ValueComputeFunctionRequest") Control.Applicative.<|>
        (Contextvalidationfunctionrequest Data.Functor.<$> v Data.Aeson..: "ContextValidationFunctionRequest") Control.Applicative.<|>
        fail "Could not parse FunctionExecutionRequest. Expected an object with one of keys: ValueValidationFunctionRequest, ValueComputeFunctionRequest, ContextValidationFunctionRequest."
    



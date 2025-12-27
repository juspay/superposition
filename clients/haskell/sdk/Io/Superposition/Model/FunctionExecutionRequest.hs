module Io.Superposition.Model.FunctionExecutionRequest (
    FunctionExecutionRequest(..)
) where
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Functor
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ChangeReasonValidationFunctionRequest
import qualified Io.Superposition.Model.ContextValidationFunctionRequest
import qualified Io.Superposition.Model.ValueComputeFunctionRequest
import qualified Io.Superposition.Model.ValueValidationFunctionRequest
import qualified Io.Superposition.Utility

-- Union implementation for FunctionExecutionRequest
data FunctionExecutionRequest =
    ValueValidate (Io.Superposition.Model.ValueValidationFunctionRequest.ValueValidationFunctionRequest)
    | ValueCompute (Io.Superposition.Model.ValueComputeFunctionRequest.ValueComputeFunctionRequest)
    | ContextValidate (Io.Superposition.Model.ContextValidationFunctionRequest.ContextValidationFunctionRequest)
    | ChangeReasonValidate (Io.Superposition.Model.ChangeReasonValidationFunctionRequest.ChangeReasonValidationFunctionRequest)
    deriving (
    GHC.Generics.Generic,
    GHC.Show.Show,
    Data.Eq.Eq
    )

instance Data.Aeson.ToJSON FunctionExecutionRequest where
    toJSON (ValueValidate a) = Data.Aeson.object [ "value_validate" Data.Aeson..= a ]
    toJSON (ValueCompute a) = Data.Aeson.object [ "value_compute" Data.Aeson..= a ]
    toJSON (ContextValidate a) = Data.Aeson.object [ "context_validate" Data.Aeson..= a ]
    toJSON (ChangeReasonValidate a) = Data.Aeson.object [ "change_reason_validate" Data.Aeson..= a ]

instance Io.Superposition.Utility.SerializeBody FunctionExecutionRequest
instance Data.Aeson.FromJSON FunctionExecutionRequest where
    parseJSON = Data.Aeson.withObject "FunctionExecutionRequest" $ \v ->
        (ValueValidate Data.Functor.<$> v Data.Aeson..: "value_validate") Control.Applicative.<|>
        (ValueCompute Data.Functor.<$> v Data.Aeson..: "value_compute") Control.Applicative.<|>
        (ContextValidate Data.Functor.<$> v Data.Aeson..: "context_validate") Control.Applicative.<|>
        (ChangeReasonValidate Data.Functor.<$> v Data.Aeson..: "change_reason_validate") Control.Applicative.<|>
        fail "Could not parse FunctionExecutionRequest. Expected an object with one of keys: value_validate, value_compute, context_validate, change_reason_validate."
    



module Io.Superposition.Model.FunctionExecutionRequest (
    FunctionExecutionRequest(..)
) where
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Functor
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.AutocompleteFunctionRequest
import qualified Io.Superposition.Model.ValidateFunctionRequest
import qualified Io.Superposition.Utility

-- Union implementation for FunctionExecutionRequest
data FunctionExecutionRequest =
    Validatefunctionrequest (Io.Superposition.Model.ValidateFunctionRequest.ValidateFunctionRequest)
    | Autocompletefunctionrequest (Io.Superposition.Model.AutocompleteFunctionRequest.AutocompleteFunctionRequest)
    deriving (
    GHC.Generics.Generic,
    GHC.Show.Show,
    Data.Eq.Eq
    )

instance Data.Aeson.ToJSON FunctionExecutionRequest where
    toJSON (Validatefunctionrequest a) = Data.Aeson.object [ "ValidateFunctionRequest" Data.Aeson..= a ]
    toJSON (Autocompletefunctionrequest a) = Data.Aeson.object [ "AutocompleteFunctionRequest" Data.Aeson..= a ]

instance Io.Superposition.Utility.SerializeBody FunctionExecutionRequest
instance Data.Aeson.FromJSON FunctionExecutionRequest where
    parseJSON = Data.Aeson.withObject "FunctionExecutionRequest" $ \v ->
        (Validatefunctionrequest Data.Functor.<$> v Data.Aeson..: "ValidateFunctionRequest") Control.Applicative.<|>
        (Autocompletefunctionrequest Data.Functor.<$> v Data.Aeson..: "AutocompleteFunctionRequest") Control.Applicative.<|>
        fail "Could not parse FunctionExecutionRequest. Expected an object with one of keys: ValidateFunctionRequest, AutocompleteFunctionRequest."
    



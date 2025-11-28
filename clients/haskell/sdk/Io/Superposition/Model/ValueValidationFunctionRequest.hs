module Io.Superposition.Model.ValueValidationFunctionRequest (
    setKey,
    setValue,
    setType',
    setEnvironment,
    build,
    ValueValidationFunctionRequestBuilder,
    ValueValidationFunctionRequest,
    key,
    value,
    type',
    environment
) where
import qualified Control.Applicative
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

data ValueValidationFunctionRequest = ValueValidationFunctionRequest {
    key :: Data.Text.Text,
    value :: Data.Aeson.Value,
    type' :: Data.Text.Text,
    environment :: Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ValueValidationFunctionRequest where
    toJSON a = Data.Aeson.object [
        "key" Data.Aeson..= key a,
        "value" Data.Aeson..= value a,
        "type" Data.Aeson..= type' a,
        "environment" Data.Aeson..= environment a
        ]
    

instance Io.Superposition.Utility.SerializeBody ValueValidationFunctionRequest

instance Data.Aeson.FromJSON ValueValidationFunctionRequest where
    parseJSON = Data.Aeson.withObject "ValueValidationFunctionRequest" $ \v -> ValueValidationFunctionRequest
        Data.Functor.<$> (v Data.Aeson..: "key")
        Control.Applicative.<*> (v Data.Aeson..: "value")
        Control.Applicative.<*> (v Data.Aeson..: "type")
        Control.Applicative.<*> (v Data.Aeson..: "environment")
    



data ValueValidationFunctionRequestBuilderState = ValueValidationFunctionRequestBuilderState {
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    valueBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    type'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    environmentBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ValueValidationFunctionRequestBuilderState
defaultBuilderState = ValueValidationFunctionRequestBuilderState {
    keyBuilderState = Data.Maybe.Nothing,
    valueBuilderState = Data.Maybe.Nothing,
    type'BuilderState = Data.Maybe.Nothing,
    environmentBuilderState = Data.Maybe.Nothing
}

type ValueValidationFunctionRequestBuilder = Control.Monad.State.Strict.State ValueValidationFunctionRequestBuilderState

setKey :: Data.Text.Text -> ValueValidationFunctionRequestBuilder ()
setKey value =
   Control.Monad.State.Strict.modify (\s -> (s { keyBuilderState = Data.Maybe.Just value }))

setValue :: Data.Aeson.Value -> ValueValidationFunctionRequestBuilder ()
setValue value =
   Control.Monad.State.Strict.modify (\s -> (s { valueBuilderState = Data.Maybe.Just value }))

setType' :: Data.Text.Text -> ValueValidationFunctionRequestBuilder ()
setType' value =
   Control.Monad.State.Strict.modify (\s -> (s { type'BuilderState = Data.Maybe.Just value }))

setEnvironment :: Data.Aeson.Value -> ValueValidationFunctionRequestBuilder ()
setEnvironment value =
   Control.Monad.State.Strict.modify (\s -> (s { environmentBuilderState = Data.Maybe.Just value }))

build :: ValueValidationFunctionRequestBuilder () -> Data.Either.Either Data.Text.Text ValueValidationFunctionRequest
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValueValidationFunctionRequest.ValueValidationFunctionRequest.key is a required property.") Data.Either.Right (keyBuilderState st)
    value' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValueValidationFunctionRequest.ValueValidationFunctionRequest.value is a required property.") Data.Either.Right (valueBuilderState st)
    type'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValueValidationFunctionRequest.ValueValidationFunctionRequest.type' is a required property.") Data.Either.Right (type'BuilderState st)
    environment' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValueValidationFunctionRequest.ValueValidationFunctionRequest.environment is a required property.") Data.Either.Right (environmentBuilderState st)
    Data.Either.Right (ValueValidationFunctionRequest { 
        key = key',
        value = value',
        type' = type'',
        environment = environment'
    })



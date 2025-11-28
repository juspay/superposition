module Io.Superposition.Model.ValueComputeFunctionRequest (
    setName,
    setPrefix,
    setType',
    setEnvironment,
    build,
    ValueComputeFunctionRequestBuilder,
    ValueComputeFunctionRequest,
    name,
    prefix,
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

data ValueComputeFunctionRequest = ValueComputeFunctionRequest {
    name :: Data.Text.Text,
    prefix :: Data.Text.Text,
    type' :: Data.Text.Text,
    environment :: Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ValueComputeFunctionRequest where
    toJSON a = Data.Aeson.object [
        "name" Data.Aeson..= name a,
        "prefix" Data.Aeson..= prefix a,
        "type" Data.Aeson..= type' a,
        "environment" Data.Aeson..= environment a
        ]
    

instance Io.Superposition.Utility.SerializeBody ValueComputeFunctionRequest

instance Data.Aeson.FromJSON ValueComputeFunctionRequest where
    parseJSON = Data.Aeson.withObject "ValueComputeFunctionRequest" $ \v -> ValueComputeFunctionRequest
        Data.Functor.<$> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "prefix")
        Control.Applicative.<*> (v Data.Aeson..: "type")
        Control.Applicative.<*> (v Data.Aeson..: "environment")
    



data ValueComputeFunctionRequestBuilderState = ValueComputeFunctionRequestBuilderState {
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    prefixBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    type'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    environmentBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ValueComputeFunctionRequestBuilderState
defaultBuilderState = ValueComputeFunctionRequestBuilderState {
    nameBuilderState = Data.Maybe.Nothing,
    prefixBuilderState = Data.Maybe.Nothing,
    type'BuilderState = Data.Maybe.Nothing,
    environmentBuilderState = Data.Maybe.Nothing
}

type ValueComputeFunctionRequestBuilder = Control.Monad.State.Strict.State ValueComputeFunctionRequestBuilderState

setName :: Data.Text.Text -> ValueComputeFunctionRequestBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setPrefix :: Data.Text.Text -> ValueComputeFunctionRequestBuilder ()
setPrefix value =
   Control.Monad.State.Strict.modify (\s -> (s { prefixBuilderState = Data.Maybe.Just value }))

setType' :: Data.Text.Text -> ValueComputeFunctionRequestBuilder ()
setType' value =
   Control.Monad.State.Strict.modify (\s -> (s { type'BuilderState = Data.Maybe.Just value }))

setEnvironment :: Data.Aeson.Value -> ValueComputeFunctionRequestBuilder ()
setEnvironment value =
   Control.Monad.State.Strict.modify (\s -> (s { environmentBuilderState = Data.Maybe.Just value }))

build :: ValueComputeFunctionRequestBuilder () -> Data.Either.Either Data.Text.Text ValueComputeFunctionRequest
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValueComputeFunctionRequest.ValueComputeFunctionRequest.name is a required property.") Data.Either.Right (nameBuilderState st)
    prefix' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValueComputeFunctionRequest.ValueComputeFunctionRequest.prefix is a required property.") Data.Either.Right (prefixBuilderState st)
    type'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValueComputeFunctionRequest.ValueComputeFunctionRequest.type' is a required property.") Data.Either.Right (type'BuilderState st)
    environment' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValueComputeFunctionRequest.ValueComputeFunctionRequest.environment is a required property.") Data.Either.Right (environmentBuilderState st)
    Data.Either.Right (ValueComputeFunctionRequest { 
        name = name',
        prefix = prefix',
        type' = type'',
        environment = environment'
    })



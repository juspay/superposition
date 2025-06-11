module Io.Superposition.Model.ValidateFunctionRequest (
    setKey,
    setValue,
    build,
    ValidateFunctionRequestBuilder,
    ValidateFunctionRequest,
    key,
    value
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

data ValidateFunctionRequest = ValidateFunctionRequest {
    key :: Data.Maybe.Maybe Data.Text.Text,
    value :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ValidateFunctionRequest where
    toJSON a = Data.Aeson.object [
        "key" Data.Aeson..= key a,
        "value" Data.Aeson..= value a
        ]
    


instance Data.Aeson.FromJSON ValidateFunctionRequest where
    parseJSON = Data.Aeson.withObject "ValidateFunctionRequest" $ \v -> ValidateFunctionRequest
        Data.Functor.<$> (v Data.Aeson..: "key")
        Control.Applicative.<*> (v Data.Aeson..: "value")
    



data ValidateFunctionRequestBuilderState = ValidateFunctionRequestBuilderState {
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    valueBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ValidateFunctionRequestBuilderState
defaultBuilderState = ValidateFunctionRequestBuilderState {
    keyBuilderState = Data.Maybe.Nothing,
    valueBuilderState = Data.Maybe.Nothing
}

newtype ValidateFunctionRequestBuilder a = ValidateFunctionRequestBuilder {
    runValidateFunctionRequestBuilder :: ValidateFunctionRequestBuilderState -> (ValidateFunctionRequestBuilderState, a)
}

instance Data.Functor.Functor ValidateFunctionRequestBuilder where
    fmap f (ValidateFunctionRequestBuilder g) =
        ValidateFunctionRequestBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ValidateFunctionRequestBuilder where
    pure a = ValidateFunctionRequestBuilder (\s -> (s, a))
    (ValidateFunctionRequestBuilder f) <*> (ValidateFunctionRequestBuilder g) = ValidateFunctionRequestBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ValidateFunctionRequestBuilder where
    (ValidateFunctionRequestBuilder f) >>= g = ValidateFunctionRequestBuilder (\s ->
        let (s', a) = f s
            (ValidateFunctionRequestBuilder h) = g a
        in h s')

setKey :: Data.Maybe.Maybe Data.Text.Text -> ValidateFunctionRequestBuilder ()
setKey value =
   ValidateFunctionRequestBuilder (\s -> (s { keyBuilderState = value }, ()))

setValue :: Data.Maybe.Maybe Data.Aeson.Value -> ValidateFunctionRequestBuilder ()
setValue value =
   ValidateFunctionRequestBuilder (\s -> (s { valueBuilderState = value }, ()))

build :: ValidateFunctionRequestBuilder () -> Data.Either.Either Data.Text.Text ValidateFunctionRequest
build builder = do
    let (st, _) = runValidateFunctionRequestBuilder builder defaultBuilderState
    key' <- Data.Either.Right (keyBuilderState st)
    value' <- Data.Either.Right (valueBuilderState st)
    Data.Either.Right (ValidateFunctionRequest { 
        key = key',
        value = value'
    })



module Io.Superposition.Model.AutocompleteFunctionRequest (
    setName,
    setPrefix,
    setEnvironment,
    build,
    AutocompleteFunctionRequestBuilder,
    AutocompleteFunctionRequest,
    name,
    prefix,
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

data AutocompleteFunctionRequest = AutocompleteFunctionRequest {
    name :: Data.Maybe.Maybe Data.Text.Text,
    prefix :: Data.Maybe.Maybe Data.Text.Text,
    environment :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON AutocompleteFunctionRequest where
    toJSON a = Data.Aeson.object [
        "name" Data.Aeson..= name a,
        "prefix" Data.Aeson..= prefix a,
        "environment" Data.Aeson..= environment a
        ]
    

instance Io.Superposition.Utility.SerializeBody AutocompleteFunctionRequest

instance Data.Aeson.FromJSON AutocompleteFunctionRequest where
    parseJSON = Data.Aeson.withObject "AutocompleteFunctionRequest" $ \v -> AutocompleteFunctionRequest
        Data.Functor.<$> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "prefix")
        Control.Applicative.<*> (v Data.Aeson..: "environment")
    



data AutocompleteFunctionRequestBuilderState = AutocompleteFunctionRequestBuilderState {
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    prefixBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    environmentBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: AutocompleteFunctionRequestBuilderState
defaultBuilderState = AutocompleteFunctionRequestBuilderState {
    nameBuilderState = Data.Maybe.Nothing,
    prefixBuilderState = Data.Maybe.Nothing,
    environmentBuilderState = Data.Maybe.Nothing
}

type AutocompleteFunctionRequestBuilder = Control.Monad.State.Strict.State AutocompleteFunctionRequestBuilderState

setName :: Data.Maybe.Maybe Data.Text.Text -> AutocompleteFunctionRequestBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = value }))

setPrefix :: Data.Maybe.Maybe Data.Text.Text -> AutocompleteFunctionRequestBuilder ()
setPrefix value =
   Control.Monad.State.Strict.modify (\s -> (s { prefixBuilderState = value }))

setEnvironment :: Data.Maybe.Maybe Data.Aeson.Value -> AutocompleteFunctionRequestBuilder ()
setEnvironment value =
   Control.Monad.State.Strict.modify (\s -> (s { environmentBuilderState = value }))

build :: AutocompleteFunctionRequestBuilder () -> Data.Either.Either Data.Text.Text AutocompleteFunctionRequest
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    name' <- Data.Either.Right (nameBuilderState st)
    prefix' <- Data.Either.Right (prefixBuilderState st)
    environment' <- Data.Either.Right (environmentBuilderState st)
    Data.Either.Right (AutocompleteFunctionRequest { 
        name = name',
        prefix = prefix',
        environment = environment'
    })



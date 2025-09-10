module Io.Superposition.Model.VariantUpdateRequest (
    setId',
    setOverrides,
    build,
    VariantUpdateRequestBuilder,
    VariantUpdateRequest,
    id',
    overrides
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

data VariantUpdateRequest = VariantUpdateRequest {
    id' :: Data.Text.Text,
    overrides :: Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON VariantUpdateRequest where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "overrides" Data.Aeson..= overrides a
        ]
    

instance Io.Superposition.Utility.SerializeBody VariantUpdateRequest

instance Data.Aeson.FromJSON VariantUpdateRequest where
    parseJSON = Data.Aeson.withObject "VariantUpdateRequest" $ \v -> VariantUpdateRequest
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "overrides")
    



data VariantUpdateRequestBuilderState = VariantUpdateRequestBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    overridesBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: VariantUpdateRequestBuilderState
defaultBuilderState = VariantUpdateRequestBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    overridesBuilderState = Data.Maybe.Nothing
}

type VariantUpdateRequestBuilder = Control.Monad.State.Strict.State VariantUpdateRequestBuilderState

setId' :: Data.Text.Text -> VariantUpdateRequestBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setOverrides :: Data.Aeson.Value -> VariantUpdateRequestBuilder ()
setOverrides value =
   Control.Monad.State.Strict.modify (\s -> (s { overridesBuilderState = Data.Maybe.Just value }))

build :: VariantUpdateRequestBuilder () -> Data.Either.Either Data.Text.Text VariantUpdateRequest
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.VariantUpdateRequest.VariantUpdateRequest.id' is a required property.") Data.Either.Right (id'BuilderState st)
    overrides' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.VariantUpdateRequest.VariantUpdateRequest.overrides is a required property.") Data.Either.Right (overridesBuilderState st)
    Data.Either.Right (VariantUpdateRequest { 
        id' = id'',
        overrides = overrides'
    })



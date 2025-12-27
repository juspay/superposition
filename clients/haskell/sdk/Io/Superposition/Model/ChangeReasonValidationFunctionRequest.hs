module Io.Superposition.Model.ChangeReasonValidationFunctionRequest (
    setChangeReason,
    build,
    ChangeReasonValidationFunctionRequestBuilder,
    ChangeReasonValidationFunctionRequest,
    change_reason
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

data ChangeReasonValidationFunctionRequest = ChangeReasonValidationFunctionRequest {
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ChangeReasonValidationFunctionRequest where
    toJSON a = Data.Aeson.object [
        "change_reason" Data.Aeson..= change_reason a
        ]
    

instance Io.Superposition.Utility.SerializeBody ChangeReasonValidationFunctionRequest

instance Data.Aeson.FromJSON ChangeReasonValidationFunctionRequest where
    parseJSON = Data.Aeson.withObject "ChangeReasonValidationFunctionRequest" $ \v -> ChangeReasonValidationFunctionRequest
        Data.Functor.<$> (v Data.Aeson..: "change_reason")
    



data ChangeReasonValidationFunctionRequestBuilderState = ChangeReasonValidationFunctionRequestBuilderState {
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ChangeReasonValidationFunctionRequestBuilderState
defaultBuilderState = ChangeReasonValidationFunctionRequestBuilderState {
    change_reasonBuilderState = Data.Maybe.Nothing
}

type ChangeReasonValidationFunctionRequestBuilder = Control.Monad.State.Strict.State ChangeReasonValidationFunctionRequestBuilderState

setChangeReason :: Data.Text.Text -> ChangeReasonValidationFunctionRequestBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

build :: ChangeReasonValidationFunctionRequestBuilder () -> Data.Either.Either Data.Text.Text ChangeReasonValidationFunctionRequest
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ChangeReasonValidationFunctionRequest.ChangeReasonValidationFunctionRequest.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (ChangeReasonValidationFunctionRequest { 
        change_reason = change_reason'
    })



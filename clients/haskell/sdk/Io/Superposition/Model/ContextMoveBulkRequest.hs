module Io.Superposition.Model.ContextMoveBulkRequest (
    setId',
    setRequest,
    build,
    ContextMoveBulkRequestBuilder,
    ContextMoveBulkRequest,
    id',
    request
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
import qualified Io.Superposition.Model.ContextMove
import qualified Io.Superposition.Utility

data ContextMoveBulkRequest = ContextMoveBulkRequest {
    id' :: Data.Text.Text,
    request :: Io.Superposition.Model.ContextMove.ContextMove
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ContextMoveBulkRequest where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "request" Data.Aeson..= request a
        ]
    

instance Io.Superposition.Utility.SerializeBody ContextMoveBulkRequest

instance Data.Aeson.FromJSON ContextMoveBulkRequest where
    parseJSON = Data.Aeson.withObject "ContextMoveBulkRequest" $ \v -> ContextMoveBulkRequest
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "request")
    



data ContextMoveBulkRequestBuilderState = ContextMoveBulkRequestBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    requestBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ContextMove.ContextMove
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ContextMoveBulkRequestBuilderState
defaultBuilderState = ContextMoveBulkRequestBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    requestBuilderState = Data.Maybe.Nothing
}

type ContextMoveBulkRequestBuilder = Control.Monad.State.Strict.State ContextMoveBulkRequestBuilderState

setId' :: Data.Text.Text -> ContextMoveBulkRequestBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setRequest :: Io.Superposition.Model.ContextMove.ContextMove -> ContextMoveBulkRequestBuilder ()
setRequest value =
   Control.Monad.State.Strict.modify (\s -> (s { requestBuilderState = Data.Maybe.Just value }))

build :: ContextMoveBulkRequestBuilder () -> Data.Either.Either Data.Text.Text ContextMoveBulkRequest
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ContextMoveBulkRequest.ContextMoveBulkRequest.id' is a required property.") Data.Either.Right (id'BuilderState st)
    request' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ContextMoveBulkRequest.ContextMoveBulkRequest.request is a required property.") Data.Either.Right (requestBuilderState st)
    Data.Either.Right (ContextMoveBulkRequest { 
        id' = id'',
        request = request'
    })



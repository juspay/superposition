module Io.Superposition.SuperpositionClient (
    SuperpositionClient,
    Io.Superposition.Utility.RawRequest,
    Io.Superposition.Utility.requestMethod,
    Io.Superposition.Utility.requestPath,
    Io.Superposition.Utility.requestQuery,
    Io.Superposition.Utility.requestHeaders,
    Io.Superposition.Utility.requestBody,
    Io.Superposition.Utility.RawResponse,
    Io.Superposition.Utility.responseStatus,
    Io.Superposition.Utility.responseHeaders,
    Io.Superposition.Utility.responseBody,
    Io.Superposition.Utility.HttpMetadata,
    Io.Superposition.Utility.rawRequest,
    Io.Superposition.Utility.rawResponse,
    endpointUri,
    httpManager,
    token,
    setEndpointuri,
    setHttpmanager,
    setToken,
    build,
    SuperpositionClientBuilder
) where
import qualified Control.Monad.State.Strict
import qualified Data.Either
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.URI

data SuperpositionClient = SuperpositionClient {
    endpointUri :: Network.URI.URI,
    httpManager :: Network.HTTP.Client.Manager,
    token :: Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

data SuperpositionClientBuilderState = SuperpositionClientBuilderState {
    endpointUriBuilderState :: Data.Maybe.Maybe Network.URI.URI,
    httpManagerBuilderState :: Data.Maybe.Maybe Network.HTTP.Client.Manager,
    tokenBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: SuperpositionClientBuilderState
defaultBuilderState = SuperpositionClientBuilderState {
    endpointUriBuilderState = Data.Maybe.Nothing,
    httpManagerBuilderState = Data.Maybe.Nothing,
    tokenBuilderState = Data.Maybe.Nothing
}

type SuperpositionClientBuilder = Control.Monad.State.Strict.State SuperpositionClientBuilderState

setEndpointuri :: Network.URI.URI -> SuperpositionClientBuilder ()
setEndpointuri value =
   Control.Monad.State.Strict.modify (\s -> (s { endpointUriBuilderState = Data.Maybe.Just value }))

setHttpmanager :: Network.HTTP.Client.Manager -> SuperpositionClientBuilder ()
setHttpmanager value =
   Control.Monad.State.Strict.modify (\s -> (s { httpManagerBuilderState = Data.Maybe.Just value }))

setToken :: Data.Text.Text -> SuperpositionClientBuilder ()
setToken value =
   Control.Monad.State.Strict.modify (\s -> (s { tokenBuilderState = Data.Maybe.Just value }))

build :: SuperpositionClientBuilder () -> Data.Either.Either Data.Text.Text SuperpositionClient
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    endpointUri' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.SuperpositionClient.SuperpositionClient.endpointUri is a required property.") Data.Either.Right (endpointUriBuilderState st)
    httpManager' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.SuperpositionClient.SuperpositionClient.httpManager is a required property.") Data.Either.Right (httpManagerBuilderState st)
    token' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.SuperpositionClient.SuperpositionClient.token is a required property.") Data.Either.Right (tokenBuilderState st)
    Data.Either.Right (SuperpositionClient { 
        endpointUri = endpointUri',
        httpManager = httpManager',
        token = token'
    })



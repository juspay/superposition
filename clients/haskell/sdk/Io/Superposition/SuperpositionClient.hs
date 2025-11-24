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
    Io.Superposition.Utility.BearerAuth(..),
    Io.Superposition.Utility.BasicAuth(..),
    endpointUri,
    httpManager,
    bearerAuth,
    basicAuth,
    setEndpointuri,
    setHttpmanager,
    setBearerauth,
    setBasicauth,
    build,
    SuperpositionClientBuilder,
    getAuth
) where
import qualified Control.Applicative
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
    bearerAuth :: Data.Maybe.Maybe Io.Superposition.Utility.BearerAuth,
    basicAuth :: Data.Maybe.Maybe Io.Superposition.Utility.BasicAuth
} deriving (
  GHC.Generics.Generic
  )

data SuperpositionClientBuilderState = SuperpositionClientBuilderState {
    endpointUriBuilderState :: Data.Maybe.Maybe Network.URI.URI,
    httpManagerBuilderState :: Data.Maybe.Maybe Network.HTTP.Client.Manager,
    bearerAuthBuilderState :: Data.Maybe.Maybe Io.Superposition.Utility.BearerAuth,
    basicAuthBuilderState :: Data.Maybe.Maybe Io.Superposition.Utility.BasicAuth
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: SuperpositionClientBuilderState
defaultBuilderState = SuperpositionClientBuilderState {
    endpointUriBuilderState = Data.Maybe.Nothing,
    httpManagerBuilderState = Data.Maybe.Nothing,
    bearerAuthBuilderState = Data.Maybe.Nothing,
    basicAuthBuilderState = Data.Maybe.Nothing
}

type SuperpositionClientBuilder = Control.Monad.State.Strict.State SuperpositionClientBuilderState

setEndpointuri :: Network.URI.URI -> SuperpositionClientBuilder ()
setEndpointuri value =
   Control.Monad.State.Strict.modify (\s -> (s { endpointUriBuilderState = Data.Maybe.Just value }))

setHttpmanager :: Network.HTTP.Client.Manager -> SuperpositionClientBuilder ()
setHttpmanager value =
   Control.Monad.State.Strict.modify (\s -> (s { httpManagerBuilderState = Data.Maybe.Just value }))

setBearerauth :: Data.Maybe.Maybe Io.Superposition.Utility.BearerAuth -> SuperpositionClientBuilder ()
setBearerauth value =
   Control.Monad.State.Strict.modify (\s -> (s { bearerAuthBuilderState = value }))

setBasicauth :: Data.Maybe.Maybe Io.Superposition.Utility.BasicAuth -> SuperpositionClientBuilder ()
setBasicauth value =
   Control.Monad.State.Strict.modify (\s -> (s { basicAuthBuilderState = value }))

build :: SuperpositionClientBuilder () -> Data.Either.Either Data.Text.Text SuperpositionClient
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    endpointUri' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.SuperpositionClient.SuperpositionClient.endpointUri is a required property.") Data.Either.Right (endpointUriBuilderState st)
    httpManager' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.SuperpositionClient.SuperpositionClient.httpManager is a required property.") Data.Either.Right (httpManagerBuilderState st)
    bearerAuth' <- Data.Either.Right (bearerAuthBuilderState st)
    basicAuth' <- Data.Either.Right (basicAuthBuilderState st)
    Data.Either.Right (SuperpositionClient { 
        endpointUri = endpointUri',
        httpManager = httpManager',
        bearerAuth = bearerAuth',
        basicAuth = basicAuth'
    })

getAuth :: SuperpositionClient -> Maybe Io.Superposition.Utility.DynAuth
getAuth client = (Nothing
    Control.Applicative.<|> (Io.Superposition.Utility.DynAuth <$> (basicAuth client))
    Control.Applicative.<|> (Io.Superposition.Utility.DynAuth <$> (bearerAuth client))
    )



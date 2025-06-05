module Io.Superposition.SuperpositionClient (
    SuperpositionClient,
    endpointUri,
    httpManager,
    token,
    setEndpointuri,
    setHttpmanager,
    setToken,
    build,
    SuperpositionClientBuilder
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
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

newtype SuperpositionClientBuilder a = SuperpositionClientBuilder {
    runSuperpositionClientBuilder :: SuperpositionClientBuilderState -> (SuperpositionClientBuilderState, a)
}

instance Data.Functor.Functor SuperpositionClientBuilder where
    fmap f (SuperpositionClientBuilder g) =
        SuperpositionClientBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative SuperpositionClientBuilder where
    pure a = SuperpositionClientBuilder (\s -> (s, a))
    (SuperpositionClientBuilder f) <*> (SuperpositionClientBuilder g) = SuperpositionClientBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad SuperpositionClientBuilder where
    (SuperpositionClientBuilder f) >>= g = SuperpositionClientBuilder (\s ->
        let (s', a) = f s
            (SuperpositionClientBuilder h) = g a
        in h s')

setEndpointuri :: Network.URI.URI -> SuperpositionClientBuilder ()
setEndpointuri value =
   SuperpositionClientBuilder (\s -> (s { endpointUriBuilderState = Data.Maybe.Just value }, ()))

setHttpmanager :: Network.HTTP.Client.Manager -> SuperpositionClientBuilder ()
setHttpmanager value =
   SuperpositionClientBuilder (\s -> (s { httpManagerBuilderState = Data.Maybe.Just value }, ()))

setToken :: Data.Text.Text -> SuperpositionClientBuilder ()
setToken value =
   SuperpositionClientBuilder (\s -> (s { tokenBuilderState = Data.Maybe.Just value }, ()))

build :: SuperpositionClientBuilder () -> Data.Either.Either Data.Text.Text SuperpositionClient
build builder = do
    let (st, _) = runSuperpositionClientBuilder builder defaultBuilderState
    endpointUri' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.SuperpositionClient.SuperpositionClient.endpointUri is a required property.") Data.Either.Right (endpointUriBuilderState st)
    httpManager' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.SuperpositionClient.SuperpositionClient.httpManager is a required property.") Data.Either.Right (httpManagerBuilderState st)
    token' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.SuperpositionClient.SuperpositionClient.token is a required property.") Data.Either.Right (tokenBuilderState st)
    Data.Either.Right (SuperpositionClient { 
        endpointUri = endpointUri',
        httpManager = httpManager',
        token = token'
    })



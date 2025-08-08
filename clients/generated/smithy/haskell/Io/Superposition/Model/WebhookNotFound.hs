module Io.Superposition.Model.WebhookNotFound (
    build,
    WebhookNotFoundBuilder,
    WebhookNotFound
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data WebhookNotFound = WebhookNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WebhookNotFound where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON WebhookNotFound where
    parseJSON = Data.Aeson.withObject "WebhookNotFound" $ \_ -> pure $ WebhookNotFound



data WebhookNotFoundBuilderState = WebhookNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WebhookNotFoundBuilderState
defaultBuilderState = WebhookNotFoundBuilderState {
}

newtype WebhookNotFoundBuilder a = WebhookNotFoundBuilder {
    runWebhookNotFoundBuilder :: WebhookNotFoundBuilderState -> (WebhookNotFoundBuilderState, a)
}

instance Data.Functor.Functor WebhookNotFoundBuilder where
    fmap f (WebhookNotFoundBuilder g) =
        WebhookNotFoundBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative WebhookNotFoundBuilder where
    pure a = WebhookNotFoundBuilder (\s -> (s, a))
    (WebhookNotFoundBuilder f) <*> (WebhookNotFoundBuilder g) = WebhookNotFoundBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad WebhookNotFoundBuilder where
    (WebhookNotFoundBuilder f) >>= g = WebhookNotFoundBuilder (\s ->
        let (s', a) = f s
            (WebhookNotFoundBuilder h) = g a
        in h s')


build :: WebhookNotFoundBuilder () -> Data.Either.Either Data.Text.Text WebhookNotFound
build builder = do
    let (st, _) = runWebhookNotFoundBuilder builder defaultBuilderState
    Data.Either.Right (WebhookNotFound { 
    })



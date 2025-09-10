module Io.Superposition.Model.WebhookNotFound (
    build,
    WebhookNotFoundBuilder,
    WebhookNotFound
) where
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data WebhookNotFound = WebhookNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WebhookNotFound where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody WebhookNotFound

instance Data.Aeson.FromJSON WebhookNotFound where
    parseJSON = Data.Aeson.withObject "WebhookNotFound" $ \_ -> pure $ WebhookNotFound



data WebhookNotFoundBuilderState = WebhookNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WebhookNotFoundBuilderState
defaultBuilderState = WebhookNotFoundBuilderState {
}

type WebhookNotFoundBuilder = Control.Monad.State.Strict.State WebhookNotFoundBuilderState


build :: WebhookNotFoundBuilder () -> Data.Either.Either Data.Text.Text WebhookNotFound
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (WebhookNotFound { 
    })


instance Io.Superposition.Utility.FromResponseParser WebhookNotFound where
    expectedStatus = Network.HTTP.Types.status404
    responseParser = do
        
        
        pure $ WebhookNotFound {
            
        }


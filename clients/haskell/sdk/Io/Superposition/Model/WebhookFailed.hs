module Io.Superposition.Model.WebhookFailed (
    setData',
    build,
    WebhookFailedBuilder,
    WebhookFailed,
    data'
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
import qualified Network.HTTP.Types

data WebhookFailed = WebhookFailed {
    data' :: Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WebhookFailed where
    toJSON a = Data.Aeson.object [
        "data" Data.Aeson..= data' a
        ]
    

instance Io.Superposition.Utility.SerializeBody WebhookFailed

instance Data.Aeson.FromJSON WebhookFailed where
    parseJSON = Data.Aeson.withObject "WebhookFailed" $ \v -> WebhookFailed
        Data.Functor.<$> (v Data.Aeson..: "data")
    



data WebhookFailedBuilderState = WebhookFailedBuilderState {
    data'BuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WebhookFailedBuilderState
defaultBuilderState = WebhookFailedBuilderState {
    data'BuilderState = Data.Maybe.Nothing
}

type WebhookFailedBuilder = Control.Monad.State.Strict.State WebhookFailedBuilderState

setData' :: Data.Aeson.Value -> WebhookFailedBuilder ()
setData' value =
   Control.Monad.State.Strict.modify (\s -> (s { data'BuilderState = Data.Maybe.Just value }))

build :: WebhookFailedBuilder () -> Data.Either.Either Data.Text.Text WebhookFailed
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    data'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WebhookFailed.WebhookFailed.data' is a required property.") Data.Either.Right (data'BuilderState st)
    Data.Either.Right (WebhookFailed { 
        data' = data''
    })


instance Io.Superposition.Utility.FromResponseParser WebhookFailed where
    expectedStatus = (Network.HTTP.Types.mkStatus 512 "")
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "data"
        pure $ WebhookFailed {
            data' = var0
        }


module Io.Superposition.Model.DeleteWebhookOutput (
    build,
    DeleteWebhookOutputBuilder,
    DeleteWebhookOutput
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

data DeleteWebhookOutput = DeleteWebhookOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteWebhookOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteWebhookOutput

instance Data.Aeson.FromJSON DeleteWebhookOutput where
    parseJSON = Data.Aeson.withObject "DeleteWebhookOutput" $ \_ -> pure $ DeleteWebhookOutput



data DeleteWebhookOutputBuilderState = DeleteWebhookOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteWebhookOutputBuilderState
defaultBuilderState = DeleteWebhookOutputBuilderState {
}

type DeleteWebhookOutputBuilder = Control.Monad.State.Strict.State DeleteWebhookOutputBuilderState


build :: DeleteWebhookOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteWebhookOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (DeleteWebhookOutput { 
    })


instance Io.Superposition.Utility.FromResponseParser DeleteWebhookOutput where
    expectedStatus = Network.HTTP.Types.status204
    responseParser = do
        
        
        pure $ DeleteWebhookOutput {
            
        }


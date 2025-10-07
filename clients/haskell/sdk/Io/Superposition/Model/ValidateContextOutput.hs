module Io.Superposition.Model.ValidateContextOutput (
    build,
    ValidateContextOutputBuilder,
    ValidateContextOutput
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

data ValidateContextOutput = ValidateContextOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ValidateContextOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody ValidateContextOutput

instance Data.Aeson.FromJSON ValidateContextOutput where
    parseJSON = Data.Aeson.withObject "ValidateContextOutput" $ \_ -> pure $ ValidateContextOutput



data ValidateContextOutputBuilderState = ValidateContextOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ValidateContextOutputBuilderState
defaultBuilderState = ValidateContextOutputBuilderState {
}

type ValidateContextOutputBuilder = Control.Monad.State.Strict.State ValidateContextOutputBuilderState


build :: ValidateContextOutputBuilder () -> Data.Either.Either Data.Text.Text ValidateContextOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (ValidateContextOutput { 
    })


instance Io.Superposition.Utility.FromResponseParser ValidateContextOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        
        pure $ ValidateContextOutput {
            
        }


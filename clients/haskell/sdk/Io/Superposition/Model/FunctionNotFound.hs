module Io.Superposition.Model.FunctionNotFound (
    build,
    FunctionNotFoundBuilder,
    FunctionNotFound
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

data FunctionNotFound = FunctionNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON FunctionNotFound where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody FunctionNotFound

instance Data.Aeson.FromJSON FunctionNotFound where
    parseJSON = Data.Aeson.withObject "FunctionNotFound" $ \_ -> pure $ FunctionNotFound



data FunctionNotFoundBuilderState = FunctionNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: FunctionNotFoundBuilderState
defaultBuilderState = FunctionNotFoundBuilderState {
}

type FunctionNotFoundBuilder = Control.Monad.State.Strict.State FunctionNotFoundBuilderState


build :: FunctionNotFoundBuilder () -> Data.Either.Either Data.Text.Text FunctionNotFound
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (FunctionNotFound { 
    })


instance Io.Superposition.Utility.FromResponseParser FunctionNotFound where
    expectedStatus = Network.HTTP.Types.status404
    responseParser = do
        
        
        pure $ FunctionNotFound {
            
        }


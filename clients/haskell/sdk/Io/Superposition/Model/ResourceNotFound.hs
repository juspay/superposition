module Io.Superposition.Model.ResourceNotFound (
    build,
    ResourceNotFoundBuilder,
    ResourceNotFound
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

data ResourceNotFound = ResourceNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ResourceNotFound where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody ResourceNotFound

instance Data.Aeson.FromJSON ResourceNotFound where
    parseJSON = Data.Aeson.withObject "ResourceNotFound" $ \_ -> pure $ ResourceNotFound



data ResourceNotFoundBuilderState = ResourceNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ResourceNotFoundBuilderState
defaultBuilderState = ResourceNotFoundBuilderState {
}

type ResourceNotFoundBuilder = Control.Monad.State.Strict.State ResourceNotFoundBuilderState


build :: ResourceNotFoundBuilder () -> Data.Either.Either Data.Text.Text ResourceNotFound
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (ResourceNotFound { 
    })


instance Io.Superposition.Utility.FromResponseParser ResourceNotFound where
    expectedStatus = Network.HTTP.Types.status404
    responseParser = do
        
        
        pure $ ResourceNotFound {
            
        }


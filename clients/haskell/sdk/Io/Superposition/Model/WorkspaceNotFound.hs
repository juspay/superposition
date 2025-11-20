module Io.Superposition.Model.WorkspaceNotFound (
    build,
    WorkspaceNotFoundBuilder,
    WorkspaceNotFound
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

data WorkspaceNotFound = WorkspaceNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WorkspaceNotFound where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody WorkspaceNotFound

instance Data.Aeson.FromJSON WorkspaceNotFound where
    parseJSON = Data.Aeson.withObject "WorkspaceNotFound" $ \_ -> pure $ WorkspaceNotFound



data WorkspaceNotFoundBuilderState = WorkspaceNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WorkspaceNotFoundBuilderState
defaultBuilderState = WorkspaceNotFoundBuilderState {
}

type WorkspaceNotFoundBuilder = Control.Monad.State.Strict.State WorkspaceNotFoundBuilderState


build :: WorkspaceNotFoundBuilder () -> Data.Either.Either Data.Text.Text WorkspaceNotFound
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (WorkspaceNotFound { 
    })


instance Io.Superposition.Utility.FromResponseParser WorkspaceNotFound where
    expectedStatus = Network.HTTP.Types.status404
    responseParser = do
        
        
        pure $ WorkspaceNotFound {
            
        }


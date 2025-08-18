module Io.Superposition.Model.DeleteContextOutput (
    build,
    DeleteContextOutputBuilder,
    DeleteContextOutput
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

data DeleteContextOutput = DeleteContextOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteContextOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteContextOutput

instance Data.Aeson.FromJSON DeleteContextOutput where
    parseJSON = Data.Aeson.withObject "DeleteContextOutput" $ \_ -> pure $ DeleteContextOutput



data DeleteContextOutputBuilderState = DeleteContextOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteContextOutputBuilderState
defaultBuilderState = DeleteContextOutputBuilderState {
}

type DeleteContextOutputBuilder = Control.Monad.State.Strict.State DeleteContextOutputBuilderState


build :: DeleteContextOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteContextOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (DeleteContextOutput { 
    })


instance Io.Superposition.Utility.FromResponseParser DeleteContextOutput where
    expectedStatus = Network.HTTP.Types.status201
    responseParser = do
        
        
        pure $ DeleteContextOutput {
            
        }


module Io.Superposition.Model.DeleteDefaultConfigOutput (
    build,
    DeleteDefaultConfigOutputBuilder,
    DeleteDefaultConfigOutput
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

data DeleteDefaultConfigOutput = DeleteDefaultConfigOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteDefaultConfigOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteDefaultConfigOutput

instance Data.Aeson.FromJSON DeleteDefaultConfigOutput where
    parseJSON = Data.Aeson.withObject "DeleteDefaultConfigOutput" $ \_ -> pure $ DeleteDefaultConfigOutput



data DeleteDefaultConfigOutputBuilderState = DeleteDefaultConfigOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteDefaultConfigOutputBuilderState
defaultBuilderState = DeleteDefaultConfigOutputBuilderState {
}

type DeleteDefaultConfigOutputBuilder = Control.Monad.State.Strict.State DeleteDefaultConfigOutputBuilderState


build :: DeleteDefaultConfigOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteDefaultConfigOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (DeleteDefaultConfigOutput { 
    })


instance Io.Superposition.Utility.FromResponseParser DeleteDefaultConfigOutput where
    expectedStatus = Network.HTTP.Types.status201
    responseParser = do
        
        
        pure $ DeleteDefaultConfigOutput {
            
        }


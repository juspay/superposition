module Io.Superposition.Model.DeleteVariableOutput (
    build,
    DeleteVariableOutputBuilder,
    DeleteVariableOutput
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

data DeleteVariableOutput = DeleteVariableOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteVariableOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteVariableOutput

instance Data.Aeson.FromJSON DeleteVariableOutput where
    parseJSON = Data.Aeson.withObject "DeleteVariableOutput" $ \_ -> pure $ DeleteVariableOutput



data DeleteVariableOutputBuilderState = DeleteVariableOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteVariableOutputBuilderState
defaultBuilderState = DeleteVariableOutputBuilderState {
}

type DeleteVariableOutputBuilder = Control.Monad.State.Strict.State DeleteVariableOutputBuilderState


build :: DeleteVariableOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteVariableOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (DeleteVariableOutput { 
    })


instance Io.Superposition.Utility.FromResponseParser DeleteVariableOutput where
    expectedStatus = Network.HTTP.Types.status204
    responseParser = do
        
        
        pure $ DeleteVariableOutput {
            
        }


module Io.Superposition.Model.DeleteFunctionOutput (
    build,
    DeleteFunctionOutputBuilder,
    DeleteFunctionOutput
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

data DeleteFunctionOutput = DeleteFunctionOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteFunctionOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteFunctionOutput

instance Data.Aeson.FromJSON DeleteFunctionOutput where
    parseJSON = Data.Aeson.withObject "DeleteFunctionOutput" $ \_ -> pure $ DeleteFunctionOutput



data DeleteFunctionOutputBuilderState = DeleteFunctionOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteFunctionOutputBuilderState
defaultBuilderState = DeleteFunctionOutputBuilderState {
}

type DeleteFunctionOutputBuilder = Control.Monad.State.Strict.State DeleteFunctionOutputBuilderState


build :: DeleteFunctionOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteFunctionOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (DeleteFunctionOutput { 
    })


instance Io.Superposition.Utility.FromResponseParser DeleteFunctionOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        
        pure $ DeleteFunctionOutput {
            
        }


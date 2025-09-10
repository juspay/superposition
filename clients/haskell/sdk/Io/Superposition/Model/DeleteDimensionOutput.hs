module Io.Superposition.Model.DeleteDimensionOutput (
    build,
    DeleteDimensionOutputBuilder,
    DeleteDimensionOutput
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

data DeleteDimensionOutput = DeleteDimensionOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteDimensionOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteDimensionOutput

instance Data.Aeson.FromJSON DeleteDimensionOutput where
    parseJSON = Data.Aeson.withObject "DeleteDimensionOutput" $ \_ -> pure $ DeleteDimensionOutput



data DeleteDimensionOutputBuilderState = DeleteDimensionOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteDimensionOutputBuilderState
defaultBuilderState = DeleteDimensionOutputBuilderState {
}

type DeleteDimensionOutputBuilder = Control.Monad.State.Strict.State DeleteDimensionOutputBuilderState


build :: DeleteDimensionOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteDimensionOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (DeleteDimensionOutput { 
    })


instance Io.Superposition.Utility.FromResponseParser DeleteDimensionOutput where
    expectedStatus = Network.HTTP.Types.status201
    responseParser = do
        
        
        pure $ DeleteDimensionOutput {
            
        }


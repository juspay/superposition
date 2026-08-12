module Io.Superposition.Model.CancelJobOutput (
    build,
    CancelJobOutputBuilder,
    CancelJobOutput
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

data CancelJobOutput = CancelJobOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CancelJobOutput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody CancelJobOutput

instance Data.Aeson.FromJSON CancelJobOutput where
    parseJSON = Data.Aeson.withObject "CancelJobOutput" $ \_ -> pure $ CancelJobOutput



data CancelJobOutputBuilderState = CancelJobOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CancelJobOutputBuilderState
defaultBuilderState = CancelJobOutputBuilderState {
}

type CancelJobOutputBuilder = Control.Monad.State.Strict.State CancelJobOutputBuilderState


build :: CancelJobOutputBuilder () -> Data.Either.Either Data.Text.Text CancelJobOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (CancelJobOutput { 
    })


instance Io.Superposition.Utility.FromResponseParser CancelJobOutput where
    expectedStatus = (Network.HTTP.Types.mkStatus 200 "")
    responseParser = do
        
        
        pure $ CancelJobOutput {
            
        }


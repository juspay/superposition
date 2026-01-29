module Io.Superposition.Model.RotateMasterKeyInput (
    build,
    RotateMasterKeyInputBuilder,
    RotateMasterKeyInput
) where
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data RotateMasterKeyInput = RotateMasterKeyInput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON RotateMasterKeyInput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody RotateMasterKeyInput

instance Data.Aeson.FromJSON RotateMasterKeyInput where
    parseJSON = Data.Aeson.withObject "RotateMasterKeyInput" $ \_ -> pure $ RotateMasterKeyInput



data RotateMasterKeyInputBuilderState = RotateMasterKeyInputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: RotateMasterKeyInputBuilderState
defaultBuilderState = RotateMasterKeyInputBuilderState {
}

type RotateMasterKeyInputBuilder = Control.Monad.State.Strict.State RotateMasterKeyInputBuilderState


build :: RotateMasterKeyInputBuilder () -> Data.Either.Either Data.Text.Text RotateMasterKeyInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (RotateMasterKeyInput { 
    })


instance Io.Superposition.Utility.IntoRequestBuilder RotateMasterKeyInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "master-key",
            "rotate"
            ]
        
        
        


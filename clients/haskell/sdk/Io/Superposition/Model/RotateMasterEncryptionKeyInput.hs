module Io.Superposition.Model.RotateMasterEncryptionKeyInput (
    build,
    RotateMasterEncryptionKeyInputBuilder,
    RotateMasterEncryptionKeyInput
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

data RotateMasterEncryptionKeyInput = RotateMasterEncryptionKeyInput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON RotateMasterEncryptionKeyInput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody RotateMasterEncryptionKeyInput

instance Data.Aeson.FromJSON RotateMasterEncryptionKeyInput where
    parseJSON = Data.Aeson.withObject "RotateMasterEncryptionKeyInput" $ \_ -> pure $ RotateMasterEncryptionKeyInput



data RotateMasterEncryptionKeyInputBuilderState = RotateMasterEncryptionKeyInputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: RotateMasterEncryptionKeyInputBuilderState
defaultBuilderState = RotateMasterEncryptionKeyInputBuilderState {
}

type RotateMasterEncryptionKeyInputBuilder = Control.Monad.State.Strict.State RotateMasterEncryptionKeyInputBuilderState


build :: RotateMasterEncryptionKeyInputBuilder () -> Data.Either.Either Data.Text.Text RotateMasterEncryptionKeyInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (RotateMasterEncryptionKeyInput { 
    })


instance Io.Superposition.Utility.IntoRequestBuilder RotateMasterEncryptionKeyInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "master-encryption-key",
            "rotate"
            ]
        
        
        


module Io.Superposition.Model.GenerateMasterKeyInput (
    build,
    GenerateMasterKeyInputBuilder,
    GenerateMasterKeyInput
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

data GenerateMasterKeyInput = GenerateMasterKeyInput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GenerateMasterKeyInput where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody GenerateMasterKeyInput

instance Data.Aeson.FromJSON GenerateMasterKeyInput where
    parseJSON = Data.Aeson.withObject "GenerateMasterKeyInput" $ \_ -> pure $ GenerateMasterKeyInput



data GenerateMasterKeyInputBuilderState = GenerateMasterKeyInputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GenerateMasterKeyInputBuilderState
defaultBuilderState = GenerateMasterKeyInputBuilderState {
}

type GenerateMasterKeyInputBuilder = Control.Monad.State.Strict.State GenerateMasterKeyInputBuilderState


build :: GenerateMasterKeyInputBuilder () -> Data.Either.Either Data.Text.Text GenerateMasterKeyInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (GenerateMasterKeyInput { 
    })


instance Io.Superposition.Utility.IntoRequestBuilder GenerateMasterKeyInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "master-key",
            "generate"
            ]
        
        
        


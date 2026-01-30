module Io.Superposition.Model.RotateWorkspaceEncryptionKeyOutput (
    setTotalSecretsReEncrypted,
    build,
    RotateWorkspaceEncryptionKeyOutputBuilder,
    RotateWorkspaceEncryptionKeyOutput,
    total_secrets_re_encrypted
) where
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data RotateWorkspaceEncryptionKeyOutput = RotateWorkspaceEncryptionKeyOutput {
    total_secrets_re_encrypted :: Data.Int.Int64
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON RotateWorkspaceEncryptionKeyOutput where
    toJSON a = Data.Aeson.object [
        "total_secrets_re_encrypted" Data.Aeson..= total_secrets_re_encrypted a
        ]
    

instance Io.Superposition.Utility.SerializeBody RotateWorkspaceEncryptionKeyOutput

instance Data.Aeson.FromJSON RotateWorkspaceEncryptionKeyOutput where
    parseJSON = Data.Aeson.withObject "RotateWorkspaceEncryptionKeyOutput" $ \v -> RotateWorkspaceEncryptionKeyOutput
        Data.Functor.<$> (v Data.Aeson..: "total_secrets_re_encrypted")
    



data RotateWorkspaceEncryptionKeyOutputBuilderState = RotateWorkspaceEncryptionKeyOutputBuilderState {
    total_secrets_re_encryptedBuilderState :: Data.Maybe.Maybe Data.Int.Int64
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: RotateWorkspaceEncryptionKeyOutputBuilderState
defaultBuilderState = RotateWorkspaceEncryptionKeyOutputBuilderState {
    total_secrets_re_encryptedBuilderState = Data.Maybe.Nothing
}

type RotateWorkspaceEncryptionKeyOutputBuilder = Control.Monad.State.Strict.State RotateWorkspaceEncryptionKeyOutputBuilderState

setTotalSecretsReEncrypted :: Data.Int.Int64 -> RotateWorkspaceEncryptionKeyOutputBuilder ()
setTotalSecretsReEncrypted value =
   Control.Monad.State.Strict.modify (\s -> (s { total_secrets_re_encryptedBuilderState = Data.Maybe.Just value }))

build :: RotateWorkspaceEncryptionKeyOutputBuilder () -> Data.Either.Either Data.Text.Text RotateWorkspaceEncryptionKeyOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    total_secrets_re_encrypted' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RotateWorkspaceEncryptionKeyOutput.RotateWorkspaceEncryptionKeyOutput.total_secrets_re_encrypted is a required property.") Data.Either.Right (total_secrets_re_encryptedBuilderState st)
    Data.Either.Right (RotateWorkspaceEncryptionKeyOutput { 
        total_secrets_re_encrypted = total_secrets_re_encrypted'
    })


instance Io.Superposition.Utility.FromResponseParser RotateWorkspaceEncryptionKeyOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "total_secrets_re_encrypted"
        pure $ RotateWorkspaceEncryptionKeyOutput {
            total_secrets_re_encrypted = var0
        }


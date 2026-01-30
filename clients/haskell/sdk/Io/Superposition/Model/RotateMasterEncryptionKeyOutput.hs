module Io.Superposition.Model.RotateMasterEncryptionKeyOutput (
    setWorkspacesRotated,
    setTotalSecretsReEncrypted,
    build,
    RotateMasterEncryptionKeyOutputBuilder,
    RotateMasterEncryptionKeyOutput,
    workspaces_rotated,
    total_secrets_re_encrypted
) where
import qualified Control.Applicative
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

data RotateMasterEncryptionKeyOutput = RotateMasterEncryptionKeyOutput {
    workspaces_rotated :: Data.Int.Int64,
    total_secrets_re_encrypted :: Data.Int.Int64
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON RotateMasterEncryptionKeyOutput where
    toJSON a = Data.Aeson.object [
        "workspaces_rotated" Data.Aeson..= workspaces_rotated a,
        "total_secrets_re_encrypted" Data.Aeson..= total_secrets_re_encrypted a
        ]
    

instance Io.Superposition.Utility.SerializeBody RotateMasterEncryptionKeyOutput

instance Data.Aeson.FromJSON RotateMasterEncryptionKeyOutput where
    parseJSON = Data.Aeson.withObject "RotateMasterEncryptionKeyOutput" $ \v -> RotateMasterEncryptionKeyOutput
        Data.Functor.<$> (v Data.Aeson..: "workspaces_rotated")
        Control.Applicative.<*> (v Data.Aeson..: "total_secrets_re_encrypted")
    



data RotateMasterEncryptionKeyOutputBuilderState = RotateMasterEncryptionKeyOutputBuilderState {
    workspaces_rotatedBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    total_secrets_re_encryptedBuilderState :: Data.Maybe.Maybe Data.Int.Int64
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: RotateMasterEncryptionKeyOutputBuilderState
defaultBuilderState = RotateMasterEncryptionKeyOutputBuilderState {
    workspaces_rotatedBuilderState = Data.Maybe.Nothing,
    total_secrets_re_encryptedBuilderState = Data.Maybe.Nothing
}

type RotateMasterEncryptionKeyOutputBuilder = Control.Monad.State.Strict.State RotateMasterEncryptionKeyOutputBuilderState

setWorkspacesRotated :: Data.Int.Int64 -> RotateMasterEncryptionKeyOutputBuilder ()
setWorkspacesRotated value =
   Control.Monad.State.Strict.modify (\s -> (s { workspaces_rotatedBuilderState = Data.Maybe.Just value }))

setTotalSecretsReEncrypted :: Data.Int.Int64 -> RotateMasterEncryptionKeyOutputBuilder ()
setTotalSecretsReEncrypted value =
   Control.Monad.State.Strict.modify (\s -> (s { total_secrets_re_encryptedBuilderState = Data.Maybe.Just value }))

build :: RotateMasterEncryptionKeyOutputBuilder () -> Data.Either.Either Data.Text.Text RotateMasterEncryptionKeyOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspaces_rotated' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RotateMasterEncryptionKeyOutput.RotateMasterEncryptionKeyOutput.workspaces_rotated is a required property.") Data.Either.Right (workspaces_rotatedBuilderState st)
    total_secrets_re_encrypted' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RotateMasterEncryptionKeyOutput.RotateMasterEncryptionKeyOutput.total_secrets_re_encrypted is a required property.") Data.Either.Right (total_secrets_re_encryptedBuilderState st)
    Data.Either.Right (RotateMasterEncryptionKeyOutput { 
        workspaces_rotated = workspaces_rotated',
        total_secrets_re_encrypted = total_secrets_re_encrypted'
    })


instance Io.Superposition.Utility.FromResponseParser RotateMasterEncryptionKeyOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "workspaces_rotated"
        var1 <- Io.Superposition.Utility.deSerField "total_secrets_re_encrypted"
        pure $ RotateMasterEncryptionKeyOutput {
            workspaces_rotated = var0,
            total_secrets_re_encrypted = var1
        }


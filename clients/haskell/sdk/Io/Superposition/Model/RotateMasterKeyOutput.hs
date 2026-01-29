module Io.Superposition.Model.RotateMasterKeyOutput (
    setWorkspacesRotated,
    setTotalSecretsReEncrypted,
    setRotatedAt,
    setNewMasterKey,
    build,
    RotateMasterKeyOutputBuilder,
    RotateMasterKeyOutput,
    workspaces_rotated,
    total_secrets_re_encrypted,
    rotated_at,
    new_master_key
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
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data RotateMasterKeyOutput = RotateMasterKeyOutput {
    workspaces_rotated :: Data.Int.Int64,
    total_secrets_re_encrypted :: Data.Int.Int64,
    rotated_at :: Data.Time.UTCTime,
    new_master_key :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON RotateMasterKeyOutput where
    toJSON a = Data.Aeson.object [
        "workspaces_rotated" Data.Aeson..= workspaces_rotated a,
        "total_secrets_re_encrypted" Data.Aeson..= total_secrets_re_encrypted a,
        "rotated_at" Data.Aeson..= rotated_at a,
        "new_master_key" Data.Aeson..= new_master_key a
        ]
    

instance Io.Superposition.Utility.SerializeBody RotateMasterKeyOutput

instance Data.Aeson.FromJSON RotateMasterKeyOutput where
    parseJSON = Data.Aeson.withObject "RotateMasterKeyOutput" $ \v -> RotateMasterKeyOutput
        Data.Functor.<$> (v Data.Aeson..: "workspaces_rotated")
        Control.Applicative.<*> (v Data.Aeson..: "total_secrets_re_encrypted")
        Control.Applicative.<*> (v Data.Aeson..: "rotated_at")
        Control.Applicative.<*> (v Data.Aeson..: "new_master_key")
    



data RotateMasterKeyOutputBuilderState = RotateMasterKeyOutputBuilderState {
    workspaces_rotatedBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    total_secrets_re_encryptedBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    rotated_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    new_master_keyBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: RotateMasterKeyOutputBuilderState
defaultBuilderState = RotateMasterKeyOutputBuilderState {
    workspaces_rotatedBuilderState = Data.Maybe.Nothing,
    total_secrets_re_encryptedBuilderState = Data.Maybe.Nothing,
    rotated_atBuilderState = Data.Maybe.Nothing,
    new_master_keyBuilderState = Data.Maybe.Nothing
}

type RotateMasterKeyOutputBuilder = Control.Monad.State.Strict.State RotateMasterKeyOutputBuilderState

setWorkspacesRotated :: Data.Int.Int64 -> RotateMasterKeyOutputBuilder ()
setWorkspacesRotated value =
   Control.Monad.State.Strict.modify (\s -> (s { workspaces_rotatedBuilderState = Data.Maybe.Just value }))

setTotalSecretsReEncrypted :: Data.Int.Int64 -> RotateMasterKeyOutputBuilder ()
setTotalSecretsReEncrypted value =
   Control.Monad.State.Strict.modify (\s -> (s { total_secrets_re_encryptedBuilderState = Data.Maybe.Just value }))

setRotatedAt :: Data.Time.UTCTime -> RotateMasterKeyOutputBuilder ()
setRotatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { rotated_atBuilderState = Data.Maybe.Just value }))

setNewMasterKey :: Data.Text.Text -> RotateMasterKeyOutputBuilder ()
setNewMasterKey value =
   Control.Monad.State.Strict.modify (\s -> (s { new_master_keyBuilderState = Data.Maybe.Just value }))

build :: RotateMasterKeyOutputBuilder () -> Data.Either.Either Data.Text.Text RotateMasterKeyOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspaces_rotated' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RotateMasterKeyOutput.RotateMasterKeyOutput.workspaces_rotated is a required property.") Data.Either.Right (workspaces_rotatedBuilderState st)
    total_secrets_re_encrypted' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RotateMasterKeyOutput.RotateMasterKeyOutput.total_secrets_re_encrypted is a required property.") Data.Either.Right (total_secrets_re_encryptedBuilderState st)
    rotated_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RotateMasterKeyOutput.RotateMasterKeyOutput.rotated_at is a required property.") Data.Either.Right (rotated_atBuilderState st)
    new_master_key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RotateMasterKeyOutput.RotateMasterKeyOutput.new_master_key is a required property.") Data.Either.Right (new_master_keyBuilderState st)
    Data.Either.Right (RotateMasterKeyOutput { 
        workspaces_rotated = workspaces_rotated',
        total_secrets_re_encrypted = total_secrets_re_encrypted',
        rotated_at = rotated_at',
        new_master_key = new_master_key'
    })


instance Io.Superposition.Utility.FromResponseParser RotateMasterKeyOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "workspaces_rotated"
        var1 <- Io.Superposition.Utility.deSerField "total_secrets_re_encrypted"
        var2 <- Io.Superposition.Utility.deSerField "new_master_key"
        var3 <- Io.Superposition.Utility.deSerField "rotated_at"
        pure $ RotateMasterKeyOutput {
            workspaces_rotated = var0,
            total_secrets_re_encrypted = var1,
            rotated_at = var3,
            new_master_key = var2
        }


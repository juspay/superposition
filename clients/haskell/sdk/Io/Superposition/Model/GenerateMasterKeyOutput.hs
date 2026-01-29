module Io.Superposition.Model.GenerateMasterKeyOutput (
    setMasterKey,
    setInstructions,
    setWarning,
    build,
    GenerateMasterKeyOutputBuilder,
    GenerateMasterKeyOutput,
    master_key,
    instructions,
    warning
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data GenerateMasterKeyOutput = GenerateMasterKeyOutput {
    master_key :: Data.Text.Text,
    instructions :: Data.Text.Text,
    warning :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GenerateMasterKeyOutput where
    toJSON a = Data.Aeson.object [
        "master_key" Data.Aeson..= master_key a,
        "instructions" Data.Aeson..= instructions a,
        "warning" Data.Aeson..= warning a
        ]
    

instance Io.Superposition.Utility.SerializeBody GenerateMasterKeyOutput

instance Data.Aeson.FromJSON GenerateMasterKeyOutput where
    parseJSON = Data.Aeson.withObject "GenerateMasterKeyOutput" $ \v -> GenerateMasterKeyOutput
        Data.Functor.<$> (v Data.Aeson..: "master_key")
        Control.Applicative.<*> (v Data.Aeson..: "instructions")
        Control.Applicative.<*> (v Data.Aeson..: "warning")
    



data GenerateMasterKeyOutputBuilderState = GenerateMasterKeyOutputBuilderState {
    master_keyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    instructionsBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    warningBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GenerateMasterKeyOutputBuilderState
defaultBuilderState = GenerateMasterKeyOutputBuilderState {
    master_keyBuilderState = Data.Maybe.Nothing,
    instructionsBuilderState = Data.Maybe.Nothing,
    warningBuilderState = Data.Maybe.Nothing
}

type GenerateMasterKeyOutputBuilder = Control.Monad.State.Strict.State GenerateMasterKeyOutputBuilderState

setMasterKey :: Data.Text.Text -> GenerateMasterKeyOutputBuilder ()
setMasterKey value =
   Control.Monad.State.Strict.modify (\s -> (s { master_keyBuilderState = Data.Maybe.Just value }))

setInstructions :: Data.Text.Text -> GenerateMasterKeyOutputBuilder ()
setInstructions value =
   Control.Monad.State.Strict.modify (\s -> (s { instructionsBuilderState = Data.Maybe.Just value }))

setWarning :: Data.Text.Text -> GenerateMasterKeyOutputBuilder ()
setWarning value =
   Control.Monad.State.Strict.modify (\s -> (s { warningBuilderState = Data.Maybe.Just value }))

build :: GenerateMasterKeyOutputBuilder () -> Data.Either.Either Data.Text.Text GenerateMasterKeyOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    master_key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GenerateMasterKeyOutput.GenerateMasterKeyOutput.master_key is a required property.") Data.Either.Right (master_keyBuilderState st)
    instructions' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GenerateMasterKeyOutput.GenerateMasterKeyOutput.instructions is a required property.") Data.Either.Right (instructionsBuilderState st)
    warning' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GenerateMasterKeyOutput.GenerateMasterKeyOutput.warning is a required property.") Data.Either.Right (warningBuilderState st)
    Data.Either.Right (GenerateMasterKeyOutput { 
        master_key = master_key',
        instructions = instructions',
        warning = warning'
    })


instance Io.Superposition.Utility.FromResponseParser GenerateMasterKeyOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "instructions"
        var1 <- Io.Superposition.Utility.deSerField "warning"
        var2 <- Io.Superposition.Utility.deSerField "master_key"
        pure $ GenerateMasterKeyOutput {
            master_key = var2,
            instructions = var0,
            warning = var1
        }


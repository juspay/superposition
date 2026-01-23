module Io.Superposition.Model.CreateSecretOutput (
    setName,
    setDescription,
    setChangeReason,
    setCreatedBy,
    setCreatedAt,
    setLastModifiedBy,
    setLastModifiedAt,
    build,
    CreateSecretOutputBuilder,
    CreateSecretOutput,
    name,
    description,
    change_reason,
    created_by,
    created_at,
    last_modified_by,
    last_modified_at
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data CreateSecretOutput = CreateSecretOutput {
    name :: Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    created_by :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateSecretOutput where
    toJSON a = Data.Aeson.object [
        "name" Data.Aeson..= name a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "created_by" Data.Aeson..= created_by a,
        "created_at" Data.Aeson..= created_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "last_modified_at" Data.Aeson..= last_modified_at a
        ]
    

instance Io.Superposition.Utility.SerializeBody CreateSecretOutput

instance Data.Aeson.FromJSON CreateSecretOutput where
    parseJSON = Data.Aeson.withObject "CreateSecretOutput" $ \v -> CreateSecretOutput
        Data.Functor.<$> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
    



data CreateSecretOutputBuilderState = CreateSecretOutputBuilderState {
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateSecretOutputBuilderState
defaultBuilderState = CreateSecretOutputBuilderState {
    nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing
}

type CreateSecretOutputBuilder = Control.Monad.State.Strict.State CreateSecretOutputBuilderState

setName :: Data.Text.Text -> CreateSecretOutputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> CreateSecretOutputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> CreateSecretOutputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> CreateSecretOutputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> CreateSecretOutputBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> CreateSecretOutputBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> CreateSecretOutputBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

build :: CreateSecretOutputBuilder () -> Data.Either.Either Data.Text.Text CreateSecretOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateSecretOutput.CreateSecretOutput.name is a required property.") Data.Either.Right (nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateSecretOutput.CreateSecretOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateSecretOutput.CreateSecretOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateSecretOutput.CreateSecretOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateSecretOutput.CreateSecretOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateSecretOutput.CreateSecretOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateSecretOutput.CreateSecretOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    Data.Either.Right (CreateSecretOutput { 
        name = name',
        description = description',
        change_reason = change_reason',
        created_by = created_by',
        created_at = created_at',
        last_modified_by = last_modified_by',
        last_modified_at = last_modified_at'
    })


instance Io.Superposition.Utility.FromResponseParser CreateSecretOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "change_reason"
        var1 <- Io.Superposition.Utility.deSerField "name"
        var2 <- Io.Superposition.Utility.deSerField "description"
        var3 <- Io.Superposition.Utility.deSerField "created_at"
        var4 <- Io.Superposition.Utility.deSerField "last_modified_by"
        var5 <- Io.Superposition.Utility.deSerField "created_by"
        var6 <- Io.Superposition.Utility.deSerField "last_modified_at"
        pure $ CreateSecretOutput {
            name = var1,
            description = var2,
            change_reason = var0,
            created_by = var5,
            created_at = var3,
            last_modified_by = var4,
            last_modified_at = var6
        }


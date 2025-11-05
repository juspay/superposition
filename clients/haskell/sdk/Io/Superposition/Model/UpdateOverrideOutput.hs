module Io.Superposition.Model.UpdateOverrideOutput (
    setId',
    setValue,
    setOverride,
    setOverrideId,
    setWeight,
    setDescription,
    setChangeReason,
    setCreatedAt,
    setCreatedBy,
    setLastModifiedAt,
    setLastModifiedBy,
    build,
    UpdateOverrideOutputBuilder,
    UpdateOverrideOutput,
    id',
    value,
    override,
    override_id,
    weight,
    description,
    change_reason,
    created_at,
    created_by,
    last_modified_at,
    last_modified_by
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data UpdateOverrideOutput = UpdateOverrideOutput {
    id' :: Data.Text.Text,
    value :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    override :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    override_id :: Data.Text.Text,
    weight :: Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    created_by :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateOverrideOutput where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "value" Data.Aeson..= value a,
        "override" Data.Aeson..= override a,
        "override_id" Data.Aeson..= override_id a,
        "weight" Data.Aeson..= weight a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "created_at" Data.Aeson..= created_at a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified_at" Data.Aeson..= last_modified_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a
        ]
    

instance Io.Superposition.Utility.SerializeBody UpdateOverrideOutput

instance Data.Aeson.FromJSON UpdateOverrideOutput where
    parseJSON = Data.Aeson.withObject "UpdateOverrideOutput" $ \v -> UpdateOverrideOutput
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "value")
        Control.Applicative.<*> (v Data.Aeson..: "override")
        Control.Applicative.<*> (v Data.Aeson..: "override_id")
        Control.Applicative.<*> (v Data.Aeson..: "weight")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
    



data UpdateOverrideOutputBuilderState = UpdateOverrideOutputBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    valueBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    overrideBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    override_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    weightBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateOverrideOutputBuilderState
defaultBuilderState = UpdateOverrideOutputBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    valueBuilderState = Data.Maybe.Nothing,
    overrideBuilderState = Data.Maybe.Nothing,
    override_idBuilderState = Data.Maybe.Nothing,
    weightBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing
}

type UpdateOverrideOutputBuilder = Control.Monad.State.Strict.State UpdateOverrideOutputBuilderState

setId' :: Data.Text.Text -> UpdateOverrideOutputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setValue :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> UpdateOverrideOutputBuilder ()
setValue value =
   Control.Monad.State.Strict.modify (\s -> (s { valueBuilderState = Data.Maybe.Just value }))

setOverride :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> UpdateOverrideOutputBuilder ()
setOverride value =
   Control.Monad.State.Strict.modify (\s -> (s { overrideBuilderState = Data.Maybe.Just value }))

setOverrideId :: Data.Text.Text -> UpdateOverrideOutputBuilder ()
setOverrideId value =
   Control.Monad.State.Strict.modify (\s -> (s { override_idBuilderState = Data.Maybe.Just value }))

setWeight :: Data.Text.Text -> UpdateOverrideOutputBuilder ()
setWeight value =
   Control.Monad.State.Strict.modify (\s -> (s { weightBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> UpdateOverrideOutputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> UpdateOverrideOutputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> UpdateOverrideOutputBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> UpdateOverrideOutputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> UpdateOverrideOutputBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> UpdateOverrideOutputBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

build :: UpdateOverrideOutputBuilder () -> Data.Either.Either Data.Text.Text UpdateOverrideOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    value' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.value is a required property.") Data.Either.Right (valueBuilderState st)
    override' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.override is a required property.") Data.Either.Right (overrideBuilderState st)
    override_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.override_id is a required property.") Data.Either.Right (override_idBuilderState st)
    weight' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.weight is a required property.") Data.Either.Right (weightBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    Data.Either.Right (UpdateOverrideOutput { 
        id' = id'',
        value = value',
        override = override',
        override_id = override_id',
        weight = weight',
        description = description',
        change_reason = change_reason',
        created_at = created_at',
        created_by = created_by',
        last_modified_at = last_modified_at',
        last_modified_by = last_modified_by'
    })


instance Io.Superposition.Utility.FromResponseParser UpdateOverrideOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "change_reason"
        var1 <- Io.Superposition.Utility.deSerField "override_id"
        var2 <- Io.Superposition.Utility.deSerField "weight"
        var3 <- Io.Superposition.Utility.deSerField "description"
        var4 <- Io.Superposition.Utility.deSerField "created_at"
        var5 <- Io.Superposition.Utility.deSerField "id"
        var6 <- Io.Superposition.Utility.deSerField "override"
        var7 <- Io.Superposition.Utility.deSerField "last_modified_by"
        var8 <- Io.Superposition.Utility.deSerField "value"
        var9 <- Io.Superposition.Utility.deSerField "created_by"
        var10 <- Io.Superposition.Utility.deSerField "last_modified_at"
        pure $ UpdateOverrideOutput {
            id' = var5,
            value = var8,
            override = var6,
            override_id = var1,
            weight = var2,
            description = var3,
            change_reason = var0,
            created_at = var4,
            created_by = var9,
            last_modified_at = var10,
            last_modified_by = var7
        }


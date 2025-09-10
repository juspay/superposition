module Io.Superposition.Model.RemoveMembersFromGroupOutput (
    setId',
    setContextHash,
    setName,
    setDescription,
    setChangeReason,
    setContext,
    setTrafficPercentage,
    setMemberExperimentIds,
    setCreatedAt,
    setCreatedBy,
    setLastModifiedAt,
    setLastModifiedBy,
    setBuckets,
    setGroupType,
    build,
    RemoveMembersFromGroupOutputBuilder,
    RemoveMembersFromGroupOutput,
    id',
    context_hash,
    name,
    description,
    change_reason,
    context,
    traffic_percentage,
    member_experiment_ids,
    created_at,
    created_by,
    last_modified_at,
    last_modified_by,
    buckets,
    group_type
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.Bucket
import qualified Io.Superposition.Model.GroupType
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data RemoveMembersFromGroupOutput = RemoveMembersFromGroupOutput {
    id' :: Data.Text.Text,
    context_hash :: Data.Text.Text,
    name :: Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    traffic_percentage :: Data.Int.Int32,
    member_experiment_ids :: [] Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    created_by :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text,
    buckets :: [] Io.Superposition.Model.Bucket.Bucket,
    group_type :: Io.Superposition.Model.GroupType.GroupType
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON RemoveMembersFromGroupOutput where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "context_hash" Data.Aeson..= context_hash a,
        "name" Data.Aeson..= name a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "context" Data.Aeson..= context a,
        "traffic_percentage" Data.Aeson..= traffic_percentage a,
        "member_experiment_ids" Data.Aeson..= member_experiment_ids a,
        "created_at" Data.Aeson..= created_at a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified_at" Data.Aeson..= last_modified_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "buckets" Data.Aeson..= buckets a,
        "group_type" Data.Aeson..= group_type a
        ]
    

instance Io.Superposition.Utility.SerializeBody RemoveMembersFromGroupOutput

instance Data.Aeson.FromJSON RemoveMembersFromGroupOutput where
    parseJSON = Data.Aeson.withObject "RemoveMembersFromGroupOutput" $ \v -> RemoveMembersFromGroupOutput
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "context_hash")
        Control.Applicative.<*> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "traffic_percentage")
        Control.Applicative.<*> (v Data.Aeson..: "member_experiment_ids")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "buckets")
        Control.Applicative.<*> (v Data.Aeson..: "group_type")
    



data RemoveMembersFromGroupOutputBuilderState = RemoveMembersFromGroupOutputBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    context_hashBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    traffic_percentageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    member_experiment_idsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    bucketsBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.Bucket.Bucket),
    group_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.GroupType.GroupType
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: RemoveMembersFromGroupOutputBuilderState
defaultBuilderState = RemoveMembersFromGroupOutputBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    context_hashBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing,
    traffic_percentageBuilderState = Data.Maybe.Nothing,
    member_experiment_idsBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    bucketsBuilderState = Data.Maybe.Nothing,
    group_typeBuilderState = Data.Maybe.Nothing
}

type RemoveMembersFromGroupOutputBuilder = Control.Monad.State.Strict.State RemoveMembersFromGroupOutputBuilderState

setId' :: Data.Text.Text -> RemoveMembersFromGroupOutputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setContextHash :: Data.Text.Text -> RemoveMembersFromGroupOutputBuilder ()
setContextHash value =
   Control.Monad.State.Strict.modify (\s -> (s { context_hashBuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> RemoveMembersFromGroupOutputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> RemoveMembersFromGroupOutputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> RemoveMembersFromGroupOutputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> RemoveMembersFromGroupOutputBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = Data.Maybe.Just value }))

setTrafficPercentage :: Data.Int.Int32 -> RemoveMembersFromGroupOutputBuilder ()
setTrafficPercentage value =
   Control.Monad.State.Strict.modify (\s -> (s { traffic_percentageBuilderState = Data.Maybe.Just value }))

setMemberExperimentIds :: [] Data.Text.Text -> RemoveMembersFromGroupOutputBuilder ()
setMemberExperimentIds value =
   Control.Monad.State.Strict.modify (\s -> (s { member_experiment_idsBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> RemoveMembersFromGroupOutputBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> RemoveMembersFromGroupOutputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> RemoveMembersFromGroupOutputBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> RemoveMembersFromGroupOutputBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

setBuckets :: [] Io.Superposition.Model.Bucket.Bucket -> RemoveMembersFromGroupOutputBuilder ()
setBuckets value =
   Control.Monad.State.Strict.modify (\s -> (s { bucketsBuilderState = Data.Maybe.Just value }))

setGroupType :: Io.Superposition.Model.GroupType.GroupType -> RemoveMembersFromGroupOutputBuilder ()
setGroupType value =
   Control.Monad.State.Strict.modify (\s -> (s { group_typeBuilderState = Data.Maybe.Just value }))

build :: RemoveMembersFromGroupOutputBuilder () -> Data.Either.Either Data.Text.Text RemoveMembersFromGroupOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    context_hash' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.context_hash is a required property.") Data.Either.Right (context_hashBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.name is a required property.") Data.Either.Right (nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.context is a required property.") Data.Either.Right (contextBuilderState st)
    traffic_percentage' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.traffic_percentage is a required property.") Data.Either.Right (traffic_percentageBuilderState st)
    member_experiment_ids' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.member_experiment_ids is a required property.") Data.Either.Right (member_experiment_idsBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    buckets' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.buckets is a required property.") Data.Either.Right (bucketsBuilderState st)
    group_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RemoveMembersFromGroupOutput.RemoveMembersFromGroupOutput.group_type is a required property.") Data.Either.Right (group_typeBuilderState st)
    Data.Either.Right (RemoveMembersFromGroupOutput { 
        id' = id'',
        context_hash = context_hash',
        name = name',
        description = description',
        change_reason = change_reason',
        context = context',
        traffic_percentage = traffic_percentage',
        member_experiment_ids = member_experiment_ids',
        created_at = created_at',
        created_by = created_by',
        last_modified_at = last_modified_at',
        last_modified_by = last_modified_by',
        buckets = buckets',
        group_type = group_type'
    })


instance Io.Superposition.Utility.FromResponseParser RemoveMembersFromGroupOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "buckets"
        var1 <- Io.Superposition.Utility.deSerField "description"
        var2 <- Io.Superposition.Utility.deSerField "created_at"
        var3 <- Io.Superposition.Utility.deSerField "last_modified_by"
        var4 <- Io.Superposition.Utility.deSerField "group_type"
        var5 <- Io.Superposition.Utility.deSerField "created_by"
        var6 <- Io.Superposition.Utility.deSerField "last_modified_at"
        var7 <- Io.Superposition.Utility.deSerField "change_reason"
        var8 <- Io.Superposition.Utility.deSerField "context_hash"
        var9 <- Io.Superposition.Utility.deSerField "traffic_percentage"
        var10 <- Io.Superposition.Utility.deSerField "name"
        var11 <- Io.Superposition.Utility.deSerField "context"
        var12 <- Io.Superposition.Utility.deSerField "member_experiment_ids"
        var13 <- Io.Superposition.Utility.deSerField "id"
        pure $ RemoveMembersFromGroupOutput {
            id' = var13,
            context_hash = var8,
            name = var10,
            description = var1,
            change_reason = var7,
            context = var11,
            traffic_percentage = var9,
            member_experiment_ids = var12,
            created_at = var2,
            created_by = var5,
            last_modified_at = var6,
            last_modified_by = var3,
            buckets = var0,
            group_type = var4
        }


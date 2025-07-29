module Io.Superposition.Model.DeleteExperimentGroupOutput (
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
    DeleteExperimentGroupOutputBuilder,
    DeleteExperimentGroupOutput,
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
import qualified Control.Monad
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
import qualified Io.Superposition.Model.Bucket
import qualified Io.Superposition.Model.GroupType

data DeleteExperimentGroupOutput = DeleteExperimentGroupOutput {
    id' :: Data.Text.Text,
    context_hash :: Data.Text.Text,
    name :: Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    traffic_percentage :: Integer,
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

instance Data.Aeson.ToJSON DeleteExperimentGroupOutput where
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
    


instance Data.Aeson.FromJSON DeleteExperimentGroupOutput where
    parseJSON = Data.Aeson.withObject "DeleteExperimentGroupOutput" $ \v -> DeleteExperimentGroupOutput
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
    



data DeleteExperimentGroupOutputBuilderState = DeleteExperimentGroupOutputBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    context_hashBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    traffic_percentageBuilderState :: Data.Maybe.Maybe Integer,
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

defaultBuilderState :: DeleteExperimentGroupOutputBuilderState
defaultBuilderState = DeleteExperimentGroupOutputBuilderState {
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

newtype DeleteExperimentGroupOutputBuilder a = DeleteExperimentGroupOutputBuilder {
    runDeleteExperimentGroupOutputBuilder :: DeleteExperimentGroupOutputBuilderState -> (DeleteExperimentGroupOutputBuilderState, a)
}

instance Data.Functor.Functor DeleteExperimentGroupOutputBuilder where
    fmap f (DeleteExperimentGroupOutputBuilder g) =
        DeleteExperimentGroupOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DeleteExperimentGroupOutputBuilder where
    pure a = DeleteExperimentGroupOutputBuilder (\s -> (s, a))
    (DeleteExperimentGroupOutputBuilder f) <*> (DeleteExperimentGroupOutputBuilder g) = DeleteExperimentGroupOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DeleteExperimentGroupOutputBuilder where
    (DeleteExperimentGroupOutputBuilder f) >>= g = DeleteExperimentGroupOutputBuilder (\s ->
        let (s', a) = f s
            (DeleteExperimentGroupOutputBuilder h) = g a
        in h s')

setId' :: Data.Text.Text -> DeleteExperimentGroupOutputBuilder ()
setId' value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setContextHash :: Data.Text.Text -> DeleteExperimentGroupOutputBuilder ()
setContextHash value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { context_hashBuilderState = Data.Maybe.Just value }, ()))

setName :: Data.Text.Text -> DeleteExperimentGroupOutputBuilder ()
setName value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> DeleteExperimentGroupOutputBuilder ()
setDescription value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> DeleteExperimentGroupOutputBuilder ()
setChangeReason value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> DeleteExperimentGroupOutputBuilder ()
setContext value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { contextBuilderState = Data.Maybe.Just value }, ()))

setTrafficPercentage :: Integer -> DeleteExperimentGroupOutputBuilder ()
setTrafficPercentage value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { traffic_percentageBuilderState = Data.Maybe.Just value }, ()))

setMemberExperimentIds :: [] Data.Text.Text -> DeleteExperimentGroupOutputBuilder ()
setMemberExperimentIds value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { member_experiment_idsBuilderState = Data.Maybe.Just value }, ()))

setCreatedAt :: Data.Time.UTCTime -> DeleteExperimentGroupOutputBuilder ()
setCreatedAt value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setCreatedBy :: Data.Text.Text -> DeleteExperimentGroupOutputBuilder ()
setCreatedBy value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedAt :: Data.Time.UTCTime -> DeleteExperimentGroupOutputBuilder ()
setLastModifiedAt value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> DeleteExperimentGroupOutputBuilder ()
setLastModifiedBy value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

setBuckets :: [] Io.Superposition.Model.Bucket.Bucket -> DeleteExperimentGroupOutputBuilder ()
setBuckets value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { bucketsBuilderState = Data.Maybe.Just value }, ()))

setGroupType :: Io.Superposition.Model.GroupType.GroupType -> DeleteExperimentGroupOutputBuilder ()
setGroupType value =
   DeleteExperimentGroupOutputBuilder (\s -> (s { group_typeBuilderState = Data.Maybe.Just value }, ()))

build :: DeleteExperimentGroupOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteExperimentGroupOutput
build builder = do
    let (st, _) = runDeleteExperimentGroupOutputBuilder builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    context_hash' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.context_hash is a required property.") Data.Either.Right (context_hashBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.name is a required property.") Data.Either.Right (nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.context is a required property.") Data.Either.Right (contextBuilderState st)
    traffic_percentage' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.traffic_percentage is a required property.") Data.Either.Right (traffic_percentageBuilderState st)
    member_experiment_ids' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.member_experiment_ids is a required property.") Data.Either.Right (member_experiment_idsBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    buckets' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.buckets is a required property.") Data.Either.Right (bucketsBuilderState st)
    group_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteExperimentGroupOutput.DeleteExperimentGroupOutput.group_type is a required property.") Data.Either.Right (group_typeBuilderState st)
    Data.Either.Right (DeleteExperimentGroupOutput { 
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



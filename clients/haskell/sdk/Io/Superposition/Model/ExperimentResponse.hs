module Io.Superposition.Model.ExperimentResponse (
    setId',
    setCreatedAt,
    setCreatedBy,
    setLastModified,
    setName,
    setExperimentType,
    setOverrideKeys,
    setStatus,
    setTrafficPercentage,
    setContext,
    setVariants,
    setLastModifiedBy,
    setChosenVariant,
    setDescription,
    setChangeReason,
    setStartedAt,
    setStartedBy,
    setMetricsUrl,
    setMetrics,
    setExperimentGroupId,
    build,
    ExperimentResponseBuilder,
    ExperimentResponse,
    id',
    created_at,
    created_by,
    last_modified,
    name,
    experiment_type,
    override_keys,
    status,
    traffic_percentage,
    context,
    variants,
    last_modified_by,
    chosen_variant,
    description,
    change_reason,
    started_at,
    started_by,
    metrics_url,
    metrics,
    experiment_group_id
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
import qualified Io.Superposition.Model.ExperimentStatusType
import qualified Io.Superposition.Model.ExperimentType
import qualified Io.Superposition.Model.Variant
import qualified Io.Superposition.Utility

data ExperimentResponse = ExperimentResponse {
    id' :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    created_by :: Data.Text.Text,
    last_modified :: Data.Time.UTCTime,
    name :: Data.Text.Text,
    experiment_type :: Io.Superposition.Model.ExperimentType.ExperimentType,
    override_keys :: [] Data.Text.Text,
    status :: Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType,
    traffic_percentage :: Data.Int.Int32,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    variants :: [] Io.Superposition.Model.Variant.Variant,
    last_modified_by :: Data.Text.Text,
    chosen_variant :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    started_at :: Data.Maybe.Maybe Data.Time.UTCTime,
    started_by :: Data.Maybe.Maybe Data.Text.Text,
    metrics_url :: Data.Maybe.Maybe Data.Text.Text,
    metrics :: Data.Maybe.Maybe Data.Aeson.Value,
    experiment_group_id :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ExperimentResponse where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "created_at" Data.Aeson..= created_at a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified" Data.Aeson..= last_modified a,
        "name" Data.Aeson..= name a,
        "experiment_type" Data.Aeson..= experiment_type a,
        "override_keys" Data.Aeson..= override_keys a,
        "status" Data.Aeson..= status a,
        "traffic_percentage" Data.Aeson..= traffic_percentage a,
        "context" Data.Aeson..= context a,
        "variants" Data.Aeson..= variants a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "chosen_variant" Data.Aeson..= chosen_variant a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "started_at" Data.Aeson..= started_at a,
        "started_by" Data.Aeson..= started_by a,
        "metrics_url" Data.Aeson..= metrics_url a,
        "metrics" Data.Aeson..= metrics a,
        "experiment_group_id" Data.Aeson..= experiment_group_id a
        ]
    

instance Io.Superposition.Utility.SerializeBody ExperimentResponse

instance Data.Aeson.FromJSON ExperimentResponse where
    parseJSON = Data.Aeson.withObject "ExperimentResponse" $ \v -> ExperimentResponse
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified")
        Control.Applicative.<*> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "experiment_type")
        Control.Applicative.<*> (v Data.Aeson..: "override_keys")
        Control.Applicative.<*> (v Data.Aeson..: "status")
        Control.Applicative.<*> (v Data.Aeson..: "traffic_percentage")
        Control.Applicative.<*> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "variants")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "chosen_variant")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "started_at")
        Control.Applicative.<*> (v Data.Aeson..: "started_by")
        Control.Applicative.<*> (v Data.Aeson..: "metrics_url")
        Control.Applicative.<*> (v Data.Aeson..: "metrics")
        Control.Applicative.<*> (v Data.Aeson..: "experiment_group_id")
    



data ExperimentResponseBuilderState = ExperimentResponseBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    experiment_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentType.ExperimentType,
    override_keysBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType,
    traffic_percentageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    variantsBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.Variant.Variant),
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    chosen_variantBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    started_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    started_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    metrics_urlBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    metricsBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    experiment_group_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ExperimentResponseBuilderState
defaultBuilderState = ExperimentResponseBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modifiedBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    experiment_typeBuilderState = Data.Maybe.Nothing,
    override_keysBuilderState = Data.Maybe.Nothing,
    statusBuilderState = Data.Maybe.Nothing,
    traffic_percentageBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing,
    variantsBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    chosen_variantBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    started_atBuilderState = Data.Maybe.Nothing,
    started_byBuilderState = Data.Maybe.Nothing,
    metrics_urlBuilderState = Data.Maybe.Nothing,
    metricsBuilderState = Data.Maybe.Nothing,
    experiment_group_idBuilderState = Data.Maybe.Nothing
}

type ExperimentResponseBuilder = Control.Monad.State.Strict.State ExperimentResponseBuilderState

setId' :: Data.Text.Text -> ExperimentResponseBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> ExperimentResponseBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> ExperimentResponseBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setLastModified :: Data.Time.UTCTime -> ExperimentResponseBuilder ()
setLastModified value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modifiedBuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> ExperimentResponseBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setExperimentType :: Io.Superposition.Model.ExperimentType.ExperimentType -> ExperimentResponseBuilder ()
setExperimentType value =
   Control.Monad.State.Strict.modify (\s -> (s { experiment_typeBuilderState = Data.Maybe.Just value }))

setOverrideKeys :: [] Data.Text.Text -> ExperimentResponseBuilder ()
setOverrideKeys value =
   Control.Monad.State.Strict.modify (\s -> (s { override_keysBuilderState = Data.Maybe.Just value }))

setStatus :: Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType -> ExperimentResponseBuilder ()
setStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { statusBuilderState = Data.Maybe.Just value }))

setTrafficPercentage :: Data.Int.Int32 -> ExperimentResponseBuilder ()
setTrafficPercentage value =
   Control.Monad.State.Strict.modify (\s -> (s { traffic_percentageBuilderState = Data.Maybe.Just value }))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> ExperimentResponseBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = Data.Maybe.Just value }))

setVariants :: [] Io.Superposition.Model.Variant.Variant -> ExperimentResponseBuilder ()
setVariants value =
   Control.Monad.State.Strict.modify (\s -> (s { variantsBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> ExperimentResponseBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

setChosenVariant :: Data.Maybe.Maybe Data.Text.Text -> ExperimentResponseBuilder ()
setChosenVariant value =
   Control.Monad.State.Strict.modify (\s -> (s { chosen_variantBuilderState = value }))

setDescription :: Data.Text.Text -> ExperimentResponseBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> ExperimentResponseBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setStartedAt :: Data.Maybe.Maybe Data.Time.UTCTime -> ExperimentResponseBuilder ()
setStartedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { started_atBuilderState = value }))

setStartedBy :: Data.Maybe.Maybe Data.Text.Text -> ExperimentResponseBuilder ()
setStartedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { started_byBuilderState = value }))

setMetricsUrl :: Data.Maybe.Maybe Data.Text.Text -> ExperimentResponseBuilder ()
setMetricsUrl value =
   Control.Monad.State.Strict.modify (\s -> (s { metrics_urlBuilderState = value }))

setMetrics :: Data.Maybe.Maybe Data.Aeson.Value -> ExperimentResponseBuilder ()
setMetrics value =
   Control.Monad.State.Strict.modify (\s -> (s { metricsBuilderState = value }))

setExperimentGroupId :: Data.Maybe.Maybe Data.Text.Text -> ExperimentResponseBuilder ()
setExperimentGroupId value =
   Control.Monad.State.Strict.modify (\s -> (s { experiment_group_idBuilderState = value }))

build :: ExperimentResponseBuilder () -> Data.Either.Either Data.Text.Text ExperimentResponse
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.id' is a required property.") Data.Either.Right (id'BuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.last_modified is a required property.") Data.Either.Right (last_modifiedBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.name is a required property.") Data.Either.Right (nameBuilderState st)
    experiment_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.experiment_type is a required property.") Data.Either.Right (experiment_typeBuilderState st)
    override_keys' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.override_keys is a required property.") Data.Either.Right (override_keysBuilderState st)
    status' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.status is a required property.") Data.Either.Right (statusBuilderState st)
    traffic_percentage' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.traffic_percentage is a required property.") Data.Either.Right (traffic_percentageBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.context is a required property.") Data.Either.Right (contextBuilderState st)
    variants' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.variants is a required property.") Data.Either.Right (variantsBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    chosen_variant' <- Data.Either.Right (chosen_variantBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentResponse.ExperimentResponse.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    started_at' <- Data.Either.Right (started_atBuilderState st)
    started_by' <- Data.Either.Right (started_byBuilderState st)
    metrics_url' <- Data.Either.Right (metrics_urlBuilderState st)
    metrics' <- Data.Either.Right (metricsBuilderState st)
    experiment_group_id' <- Data.Either.Right (experiment_group_idBuilderState st)
    Data.Either.Right (ExperimentResponse { 
        id' = id'',
        created_at = created_at',
        created_by = created_by',
        last_modified = last_modified',
        name = name',
        experiment_type = experiment_type',
        override_keys = override_keys',
        status = status',
        traffic_percentage = traffic_percentage',
        context = context',
        variants = variants',
        last_modified_by = last_modified_by',
        chosen_variant = chosen_variant',
        description = description',
        change_reason = change_reason',
        started_at = started_at',
        started_by = started_by',
        metrics_url = metrics_url',
        metrics = metrics',
        experiment_group_id = experiment_group_id'
    })



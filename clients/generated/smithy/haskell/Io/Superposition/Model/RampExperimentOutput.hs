module Io.Superposition.Model.RampExperimentOutput (
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
    RampExperimentOutputBuilder,
    RampExperimentOutput,
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
import qualified Io.Superposition.Model.ExperimentStatusType
import qualified Io.Superposition.Model.ExperimentType
import qualified Io.Superposition.Model.Variant

data RampExperimentOutput = RampExperimentOutput {
    id' :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    created_by :: Data.Text.Text,
    last_modified :: Data.Time.UTCTime,
    name :: Data.Text.Text,
    experiment_type :: Io.Superposition.Model.ExperimentType.ExperimentType,
    override_keys :: [] Data.Text.Text,
    status :: Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType,
    traffic_percentage :: Integer,
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

instance Data.Aeson.ToJSON RampExperimentOutput where
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
    


instance Data.Aeson.FromJSON RampExperimentOutput where
    parseJSON = Data.Aeson.withObject "RampExperimentOutput" $ \v -> RampExperimentOutput
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
    



data RampExperimentOutputBuilderState = RampExperimentOutputBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    experiment_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentType.ExperimentType,
    override_keysBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType,
    traffic_percentageBuilderState :: Data.Maybe.Maybe Integer,
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

defaultBuilderState :: RampExperimentOutputBuilderState
defaultBuilderState = RampExperimentOutputBuilderState {
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

newtype RampExperimentOutputBuilder a = RampExperimentOutputBuilder {
    runRampExperimentOutputBuilder :: RampExperimentOutputBuilderState -> (RampExperimentOutputBuilderState, a)
}

instance Data.Functor.Functor RampExperimentOutputBuilder where
    fmap f (RampExperimentOutputBuilder g) =
        RampExperimentOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative RampExperimentOutputBuilder where
    pure a = RampExperimentOutputBuilder (\s -> (s, a))
    (RampExperimentOutputBuilder f) <*> (RampExperimentOutputBuilder g) = RampExperimentOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad RampExperimentOutputBuilder where
    (RampExperimentOutputBuilder f) >>= g = RampExperimentOutputBuilder (\s ->
        let (s', a) = f s
            (RampExperimentOutputBuilder h) = g a
        in h s')

setId' :: Data.Text.Text -> RampExperimentOutputBuilder ()
setId' value =
   RampExperimentOutputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setCreatedAt :: Data.Time.UTCTime -> RampExperimentOutputBuilder ()
setCreatedAt value =
   RampExperimentOutputBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setCreatedBy :: Data.Text.Text -> RampExperimentOutputBuilder ()
setCreatedBy value =
   RampExperimentOutputBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setLastModified :: Data.Time.UTCTime -> RampExperimentOutputBuilder ()
setLastModified value =
   RampExperimentOutputBuilder (\s -> (s { last_modifiedBuilderState = Data.Maybe.Just value }, ()))

setName :: Data.Text.Text -> RampExperimentOutputBuilder ()
setName value =
   RampExperimentOutputBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

setExperimentType :: Io.Superposition.Model.ExperimentType.ExperimentType -> RampExperimentOutputBuilder ()
setExperimentType value =
   RampExperimentOutputBuilder (\s -> (s { experiment_typeBuilderState = Data.Maybe.Just value }, ()))

setOverrideKeys :: [] Data.Text.Text -> RampExperimentOutputBuilder ()
setOverrideKeys value =
   RampExperimentOutputBuilder (\s -> (s { override_keysBuilderState = Data.Maybe.Just value }, ()))

setStatus :: Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType -> RampExperimentOutputBuilder ()
setStatus value =
   RampExperimentOutputBuilder (\s -> (s { statusBuilderState = Data.Maybe.Just value }, ()))

setTrafficPercentage :: Integer -> RampExperimentOutputBuilder ()
setTrafficPercentage value =
   RampExperimentOutputBuilder (\s -> (s { traffic_percentageBuilderState = Data.Maybe.Just value }, ()))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> RampExperimentOutputBuilder ()
setContext value =
   RampExperimentOutputBuilder (\s -> (s { contextBuilderState = Data.Maybe.Just value }, ()))

setVariants :: [] Io.Superposition.Model.Variant.Variant -> RampExperimentOutputBuilder ()
setVariants value =
   RampExperimentOutputBuilder (\s -> (s { variantsBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> RampExperimentOutputBuilder ()
setLastModifiedBy value =
   RampExperimentOutputBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

setChosenVariant :: Data.Maybe.Maybe Data.Text.Text -> RampExperimentOutputBuilder ()
setChosenVariant value =
   RampExperimentOutputBuilder (\s -> (s { chosen_variantBuilderState = value }, ()))

setDescription :: Data.Text.Text -> RampExperimentOutputBuilder ()
setDescription value =
   RampExperimentOutputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> RampExperimentOutputBuilder ()
setChangeReason value =
   RampExperimentOutputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setStartedAt :: Data.Maybe.Maybe Data.Time.UTCTime -> RampExperimentOutputBuilder ()
setStartedAt value =
   RampExperimentOutputBuilder (\s -> (s { started_atBuilderState = value }, ()))

setStartedBy :: Data.Maybe.Maybe Data.Text.Text -> RampExperimentOutputBuilder ()
setStartedBy value =
   RampExperimentOutputBuilder (\s -> (s { started_byBuilderState = value }, ()))

setMetricsUrl :: Data.Maybe.Maybe Data.Text.Text -> RampExperimentOutputBuilder ()
setMetricsUrl value =
   RampExperimentOutputBuilder (\s -> (s { metrics_urlBuilderState = value }, ()))

setMetrics :: Data.Maybe.Maybe Data.Aeson.Value -> RampExperimentOutputBuilder ()
setMetrics value =
   RampExperimentOutputBuilder (\s -> (s { metricsBuilderState = value }, ()))

setExperimentGroupId :: Data.Maybe.Maybe Data.Text.Text -> RampExperimentOutputBuilder ()
setExperimentGroupId value =
   RampExperimentOutputBuilder (\s -> (s { experiment_group_idBuilderState = value }, ()))

build :: RampExperimentOutputBuilder () -> Data.Either.Either Data.Text.Text RampExperimentOutput
build builder = do
    let (st, _) = runRampExperimentOutputBuilder builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.last_modified is a required property.") Data.Either.Right (last_modifiedBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.name is a required property.") Data.Either.Right (nameBuilderState st)
    experiment_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.experiment_type is a required property.") Data.Either.Right (experiment_typeBuilderState st)
    override_keys' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.override_keys is a required property.") Data.Either.Right (override_keysBuilderState st)
    status' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.status is a required property.") Data.Either.Right (statusBuilderState st)
    traffic_percentage' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.traffic_percentage is a required property.") Data.Either.Right (traffic_percentageBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.context is a required property.") Data.Either.Right (contextBuilderState st)
    variants' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.variants is a required property.") Data.Either.Right (variantsBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    chosen_variant' <- Data.Either.Right (chosen_variantBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentOutput.RampExperimentOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    started_at' <- Data.Either.Right (started_atBuilderState st)
    started_by' <- Data.Either.Right (started_byBuilderState st)
    metrics_url' <- Data.Either.Right (metrics_urlBuilderState st)
    metrics' <- Data.Either.Right (metricsBuilderState st)
    experiment_group_id' <- Data.Either.Right (experiment_group_idBuilderState st)
    Data.Either.Right (RampExperimentOutput { 
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



module Io.Superposition.Model.ExperimentGroupResponse (
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
    build,
    ExperimentGroupResponseBuilder,
    ExperimentGroupResponse,
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
    last_modified_by
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

data ExperimentGroupResponse = ExperimentGroupResponse {
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
    last_modified_by :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ExperimentGroupResponse where
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
        "last_modified_by" Data.Aeson..= last_modified_by a
        ]
    


instance Data.Aeson.FromJSON ExperimentGroupResponse where
    parseJSON = Data.Aeson.withObject "ExperimentGroupResponse" $ \v -> ExperimentGroupResponse
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
    



data ExperimentGroupResponseBuilderState = ExperimentGroupResponseBuilderState {
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
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ExperimentGroupResponseBuilderState
defaultBuilderState = ExperimentGroupResponseBuilderState {
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
    last_modified_byBuilderState = Data.Maybe.Nothing
}

newtype ExperimentGroupResponseBuilder a = ExperimentGroupResponseBuilder {
    runExperimentGroupResponseBuilder :: ExperimentGroupResponseBuilderState -> (ExperimentGroupResponseBuilderState, a)
}

instance Data.Functor.Functor ExperimentGroupResponseBuilder where
    fmap f (ExperimentGroupResponseBuilder g) =
        ExperimentGroupResponseBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ExperimentGroupResponseBuilder where
    pure a = ExperimentGroupResponseBuilder (\s -> (s, a))
    (ExperimentGroupResponseBuilder f) <*> (ExperimentGroupResponseBuilder g) = ExperimentGroupResponseBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ExperimentGroupResponseBuilder where
    (ExperimentGroupResponseBuilder f) >>= g = ExperimentGroupResponseBuilder (\s ->
        let (s', a) = f s
            (ExperimentGroupResponseBuilder h) = g a
        in h s')

setId' :: Data.Text.Text -> ExperimentGroupResponseBuilder ()
setId' value =
   ExperimentGroupResponseBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setContextHash :: Data.Text.Text -> ExperimentGroupResponseBuilder ()
setContextHash value =
   ExperimentGroupResponseBuilder (\s -> (s { context_hashBuilderState = Data.Maybe.Just value }, ()))

setName :: Data.Text.Text -> ExperimentGroupResponseBuilder ()
setName value =
   ExperimentGroupResponseBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> ExperimentGroupResponseBuilder ()
setDescription value =
   ExperimentGroupResponseBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> ExperimentGroupResponseBuilder ()
setChangeReason value =
   ExperimentGroupResponseBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> ExperimentGroupResponseBuilder ()
setContext value =
   ExperimentGroupResponseBuilder (\s -> (s { contextBuilderState = Data.Maybe.Just value }, ()))

setTrafficPercentage :: Integer -> ExperimentGroupResponseBuilder ()
setTrafficPercentage value =
   ExperimentGroupResponseBuilder (\s -> (s { traffic_percentageBuilderState = Data.Maybe.Just value }, ()))

setMemberExperimentIds :: [] Data.Text.Text -> ExperimentGroupResponseBuilder ()
setMemberExperimentIds value =
   ExperimentGroupResponseBuilder (\s -> (s { member_experiment_idsBuilderState = Data.Maybe.Just value }, ()))

setCreatedAt :: Data.Time.UTCTime -> ExperimentGroupResponseBuilder ()
setCreatedAt value =
   ExperimentGroupResponseBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setCreatedBy :: Data.Text.Text -> ExperimentGroupResponseBuilder ()
setCreatedBy value =
   ExperimentGroupResponseBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedAt :: Data.Time.UTCTime -> ExperimentGroupResponseBuilder ()
setLastModifiedAt value =
   ExperimentGroupResponseBuilder (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> ExperimentGroupResponseBuilder ()
setLastModifiedBy value =
   ExperimentGroupResponseBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

build :: ExperimentGroupResponseBuilder () -> Data.Either.Either Data.Text.Text ExperimentGroupResponse
build builder = do
    let (st, _) = runExperimentGroupResponseBuilder builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.id' is a required property.") Data.Either.Right (id'BuilderState st)
    context_hash' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.context_hash is a required property.") Data.Either.Right (context_hashBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.name is a required property.") Data.Either.Right (nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.context is a required property.") Data.Either.Right (contextBuilderState st)
    traffic_percentage' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.traffic_percentage is a required property.") Data.Either.Right (traffic_percentageBuilderState st)
    member_experiment_ids' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.member_experiment_ids is a required property.") Data.Either.Right (member_experiment_idsBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    Data.Either.Right (ExperimentGroupResponse { 
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
        last_modified_by = last_modified_by'
    })



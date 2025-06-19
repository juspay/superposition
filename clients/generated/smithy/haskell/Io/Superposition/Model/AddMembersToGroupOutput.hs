module Io.Superposition.Model.AddMembersToGroupOutput (
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
    AddMembersToGroupOutputBuilder,
    AddMembersToGroupOutput,
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

data AddMembersToGroupOutput = AddMembersToGroupOutput {
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

instance Data.Aeson.ToJSON AddMembersToGroupOutput where
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
    


instance Data.Aeson.FromJSON AddMembersToGroupOutput where
    parseJSON = Data.Aeson.withObject "AddMembersToGroupOutput" $ \v -> AddMembersToGroupOutput
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
    



data AddMembersToGroupOutputBuilderState = AddMembersToGroupOutputBuilderState {
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

defaultBuilderState :: AddMembersToGroupOutputBuilderState
defaultBuilderState = AddMembersToGroupOutputBuilderState {
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

newtype AddMembersToGroupOutputBuilder a = AddMembersToGroupOutputBuilder {
    runAddMembersToGroupOutputBuilder :: AddMembersToGroupOutputBuilderState -> (AddMembersToGroupOutputBuilderState, a)
}

instance Data.Functor.Functor AddMembersToGroupOutputBuilder where
    fmap f (AddMembersToGroupOutputBuilder g) =
        AddMembersToGroupOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative AddMembersToGroupOutputBuilder where
    pure a = AddMembersToGroupOutputBuilder (\s -> (s, a))
    (AddMembersToGroupOutputBuilder f) <*> (AddMembersToGroupOutputBuilder g) = AddMembersToGroupOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad AddMembersToGroupOutputBuilder where
    (AddMembersToGroupOutputBuilder f) >>= g = AddMembersToGroupOutputBuilder (\s ->
        let (s', a) = f s
            (AddMembersToGroupOutputBuilder h) = g a
        in h s')

setId' :: Data.Text.Text -> AddMembersToGroupOutputBuilder ()
setId' value =
   AddMembersToGroupOutputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setContextHash :: Data.Text.Text -> AddMembersToGroupOutputBuilder ()
setContextHash value =
   AddMembersToGroupOutputBuilder (\s -> (s { context_hashBuilderState = Data.Maybe.Just value }, ()))

setName :: Data.Text.Text -> AddMembersToGroupOutputBuilder ()
setName value =
   AddMembersToGroupOutputBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> AddMembersToGroupOutputBuilder ()
setDescription value =
   AddMembersToGroupOutputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> AddMembersToGroupOutputBuilder ()
setChangeReason value =
   AddMembersToGroupOutputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> AddMembersToGroupOutputBuilder ()
setContext value =
   AddMembersToGroupOutputBuilder (\s -> (s { contextBuilderState = Data.Maybe.Just value }, ()))

setTrafficPercentage :: Integer -> AddMembersToGroupOutputBuilder ()
setTrafficPercentage value =
   AddMembersToGroupOutputBuilder (\s -> (s { traffic_percentageBuilderState = Data.Maybe.Just value }, ()))

setMemberExperimentIds :: [] Data.Text.Text -> AddMembersToGroupOutputBuilder ()
setMemberExperimentIds value =
   AddMembersToGroupOutputBuilder (\s -> (s { member_experiment_idsBuilderState = Data.Maybe.Just value }, ()))

setCreatedAt :: Data.Time.UTCTime -> AddMembersToGroupOutputBuilder ()
setCreatedAt value =
   AddMembersToGroupOutputBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setCreatedBy :: Data.Text.Text -> AddMembersToGroupOutputBuilder ()
setCreatedBy value =
   AddMembersToGroupOutputBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedAt :: Data.Time.UTCTime -> AddMembersToGroupOutputBuilder ()
setLastModifiedAt value =
   AddMembersToGroupOutputBuilder (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> AddMembersToGroupOutputBuilder ()
setLastModifiedBy value =
   AddMembersToGroupOutputBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

build :: AddMembersToGroupOutputBuilder () -> Data.Either.Either Data.Text.Text AddMembersToGroupOutput
build builder = do
    let (st, _) = runAddMembersToGroupOutputBuilder builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    context_hash' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.context_hash is a required property.") Data.Either.Right (context_hashBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.name is a required property.") Data.Either.Right (nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.context is a required property.") Data.Either.Right (contextBuilderState st)
    traffic_percentage' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.traffic_percentage is a required property.") Data.Either.Right (traffic_percentageBuilderState st)
    member_experiment_ids' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.member_experiment_ids is a required property.") Data.Either.Right (member_experiment_idsBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.AddMembersToGroupOutput.AddMembersToGroupOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    Data.Either.Right (AddMembersToGroupOutput { 
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



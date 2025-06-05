module Io.Superposition.Model.GetContextOutput (
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
    GetContextOutputBuilder,
    GetContextOutput,
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

data GetContextOutput = GetContextOutput {
    id' :: Data.Text.Text,
    value :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    override :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    override_id :: Data.Maybe.Maybe Data.Text.Text,
    weight :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Maybe.Maybe Data.Text.Text,
    created_at :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_by :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_at :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_by :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetContextOutput where
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
    


instance Data.Aeson.FromJSON GetContextOutput where
    parseJSON = Data.Aeson.withObject "GetContextOutput" $ \v -> GetContextOutput
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
    



data GetContextOutputBuilderState = GetContextOutputBuilderState {
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

defaultBuilderState :: GetContextOutputBuilderState
defaultBuilderState = GetContextOutputBuilderState {
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

newtype GetContextOutputBuilder a = GetContextOutputBuilder {
    runGetContextOutputBuilder :: GetContextOutputBuilderState -> (GetContextOutputBuilderState, a)
}

instance Data.Functor.Functor GetContextOutputBuilder where
    fmap f (GetContextOutputBuilder g) =
        GetContextOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetContextOutputBuilder where
    pure a = GetContextOutputBuilder (\s -> (s, a))
    (GetContextOutputBuilder f) <*> (GetContextOutputBuilder g) = GetContextOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetContextOutputBuilder where
    (GetContextOutputBuilder f) >>= g = GetContextOutputBuilder (\s ->
        let (s', a) = f s
            (GetContextOutputBuilder h) = g a
        in h s')

setId' :: Data.Text.Text -> GetContextOutputBuilder ()
setId' value =
   GetContextOutputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setValue :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> GetContextOutputBuilder ()
setValue value =
   GetContextOutputBuilder (\s -> (s { valueBuilderState = value }, ()))

setOverride :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> GetContextOutputBuilder ()
setOverride value =
   GetContextOutputBuilder (\s -> (s { overrideBuilderState = value }, ()))

setOverrideId :: Data.Maybe.Maybe Data.Text.Text -> GetContextOutputBuilder ()
setOverrideId value =
   GetContextOutputBuilder (\s -> (s { override_idBuilderState = value }, ()))

setWeight :: Data.Maybe.Maybe Data.Text.Text -> GetContextOutputBuilder ()
setWeight value =
   GetContextOutputBuilder (\s -> (s { weightBuilderState = value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> GetContextOutputBuilder ()
setDescription value =
   GetContextOutputBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setChangeReason :: Data.Maybe.Maybe Data.Text.Text -> GetContextOutputBuilder ()
setChangeReason value =
   GetContextOutputBuilder (\s -> (s { change_reasonBuilderState = value }, ()))

setCreatedAt :: Data.Maybe.Maybe Data.Time.UTCTime -> GetContextOutputBuilder ()
setCreatedAt value =
   GetContextOutputBuilder (\s -> (s { created_atBuilderState = value }, ()))

setCreatedBy :: Data.Maybe.Maybe Data.Text.Text -> GetContextOutputBuilder ()
setCreatedBy value =
   GetContextOutputBuilder (\s -> (s { created_byBuilderState = value }, ()))

setLastModifiedAt :: Data.Maybe.Maybe Data.Time.UTCTime -> GetContextOutputBuilder ()
setLastModifiedAt value =
   GetContextOutputBuilder (\s -> (s { last_modified_atBuilderState = value }, ()))

setLastModifiedBy :: Data.Maybe.Maybe Data.Text.Text -> GetContextOutputBuilder ()
setLastModifiedBy value =
   GetContextOutputBuilder (\s -> (s { last_modified_byBuilderState = value }, ()))

build :: GetContextOutputBuilder () -> Data.Either.Either Data.Text.Text GetContextOutput
build builder = do
    let (st, _) = runGetContextOutputBuilder builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetContextOutput.GetContextOutput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    value' <- Data.Either.Right (valueBuilderState st)
    override' <- Data.Either.Right (overrideBuilderState st)
    override_id' <- Data.Either.Right (override_idBuilderState st)
    weight' <- Data.Either.Right (weightBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Either.Right (change_reasonBuilderState st)
    created_at' <- Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Either.Right (created_byBuilderState st)
    last_modified_at' <- Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Either.Right (last_modified_byBuilderState st)
    Data.Either.Right (GetContextOutput { 
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



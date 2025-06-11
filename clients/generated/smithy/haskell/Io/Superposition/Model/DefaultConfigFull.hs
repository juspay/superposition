module Io.Superposition.Model.DefaultConfigFull (
    setKey,
    setValue,
    setSchema,
    setDescription,
    setChangeReason,
    setFunctionName,
    setCreatedAt,
    setCreatedBy,
    setLastModifiedAt,
    setLastModifiedBy,
    build,
    DefaultConfigFullBuilder,
    DefaultConfigFull,
    key,
    value,
    schema,
    description,
    change_reason,
    function_name,
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
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show

data DefaultConfigFull = DefaultConfigFull {
    key :: Data.Text.Text,
    value :: Data.Aeson.Value,
    schema :: Data.Aeson.Value,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    function_name :: Data.Maybe.Maybe Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    created_by :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DefaultConfigFull where
    toJSON a = Data.Aeson.object [
        "key" Data.Aeson..= key a,
        "value" Data.Aeson..= value a,
        "schema" Data.Aeson..= schema a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "function_name" Data.Aeson..= function_name a,
        "created_at" Data.Aeson..= created_at a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified_at" Data.Aeson..= last_modified_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a
        ]
    


instance Data.Aeson.FromJSON DefaultConfigFull where
    parseJSON = Data.Aeson.withObject "DefaultConfigFull" $ \v -> DefaultConfigFull
        Data.Functor.<$> (v Data.Aeson..: "key")
        Control.Applicative.<*> (v Data.Aeson..: "value")
        Control.Applicative.<*> (v Data.Aeson..: "schema")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
    



data DefaultConfigFullBuilderState = DefaultConfigFullBuilderState {
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    valueBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    schemaBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DefaultConfigFullBuilderState
defaultBuilderState = DefaultConfigFullBuilderState {
    keyBuilderState = Data.Maybe.Nothing,
    valueBuilderState = Data.Maybe.Nothing,
    schemaBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing
}

newtype DefaultConfigFullBuilder a = DefaultConfigFullBuilder {
    runDefaultConfigFullBuilder :: DefaultConfigFullBuilderState -> (DefaultConfigFullBuilderState, a)
}

instance Data.Functor.Functor DefaultConfigFullBuilder where
    fmap f (DefaultConfigFullBuilder g) =
        DefaultConfigFullBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DefaultConfigFullBuilder where
    pure a = DefaultConfigFullBuilder (\s -> (s, a))
    (DefaultConfigFullBuilder f) <*> (DefaultConfigFullBuilder g) = DefaultConfigFullBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DefaultConfigFullBuilder where
    (DefaultConfigFullBuilder f) >>= g = DefaultConfigFullBuilder (\s ->
        let (s', a) = f s
            (DefaultConfigFullBuilder h) = g a
        in h s')

setKey :: Data.Text.Text -> DefaultConfigFullBuilder ()
setKey value =
   DefaultConfigFullBuilder (\s -> (s { keyBuilderState = Data.Maybe.Just value }, ()))

setValue :: Data.Aeson.Value -> DefaultConfigFullBuilder ()
setValue value =
   DefaultConfigFullBuilder (\s -> (s { valueBuilderState = Data.Maybe.Just value }, ()))

setSchema :: Data.Aeson.Value -> DefaultConfigFullBuilder ()
setSchema value =
   DefaultConfigFullBuilder (\s -> (s { schemaBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> DefaultConfigFullBuilder ()
setDescription value =
   DefaultConfigFullBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> DefaultConfigFullBuilder ()
setChangeReason value =
   DefaultConfigFullBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setFunctionName :: Data.Maybe.Maybe Data.Text.Text -> DefaultConfigFullBuilder ()
setFunctionName value =
   DefaultConfigFullBuilder (\s -> (s { function_nameBuilderState = value }, ()))

setCreatedAt :: Data.Time.UTCTime -> DefaultConfigFullBuilder ()
setCreatedAt value =
   DefaultConfigFullBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setCreatedBy :: Data.Text.Text -> DefaultConfigFullBuilder ()
setCreatedBy value =
   DefaultConfigFullBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedAt :: Data.Time.UTCTime -> DefaultConfigFullBuilder ()
setLastModifiedAt value =
   DefaultConfigFullBuilder (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> DefaultConfigFullBuilder ()
setLastModifiedBy value =
   DefaultConfigFullBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

build :: DefaultConfigFullBuilder () -> Data.Either.Either Data.Text.Text DefaultConfigFull
build builder = do
    let (st, _) = runDefaultConfigFullBuilder builder defaultBuilderState
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull.key is a required property.") Data.Either.Right (keyBuilderState st)
    value' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull.value is a required property.") Data.Either.Right (valueBuilderState st)
    schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull.schema is a required property.") Data.Either.Right (schemaBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    function_name' <- Data.Either.Right (function_nameBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    Data.Either.Right (DefaultConfigFull { 
        key = key',
        value = value',
        schema = schema',
        description = description',
        change_reason = change_reason',
        function_name = function_name',
        created_at = created_at',
        created_by = created_by',
        last_modified_at = last_modified_at',
        last_modified_by = last_modified_by'
    })



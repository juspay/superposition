module Io.Superposition.Model.UpdateDefaultConfigOutput (
    setKey,
    setValue,
    setSchema,
    setDescription,
    setChangeReason,
    setFunctionName,
    setAutocompleteFunctionName,
    setCreatedAt,
    setCreatedBy,
    setLastModifiedAt,
    setLastModifiedBy,
    build,
    UpdateDefaultConfigOutputBuilder,
    UpdateDefaultConfigOutput,
    key,
    value,
    schema,
    description,
    change_reason,
    function_name,
    autocomplete_function_name,
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

data UpdateDefaultConfigOutput = UpdateDefaultConfigOutput {
    key :: Data.Text.Text,
    value :: Data.Aeson.Value,
    schema :: Data.Aeson.Value,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    function_name :: Data.Maybe.Maybe Data.Text.Text,
    autocomplete_function_name :: Data.Maybe.Maybe Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    created_by :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateDefaultConfigOutput where
    toJSON a = Data.Aeson.object [
        "key" Data.Aeson..= key a,
        "value" Data.Aeson..= value a,
        "schema" Data.Aeson..= schema a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "function_name" Data.Aeson..= function_name a,
        "autocomplete_function_name" Data.Aeson..= autocomplete_function_name a,
        "created_at" Data.Aeson..= created_at a,
        "created_by" Data.Aeson..= created_by a,
        "last_modified_at" Data.Aeson..= last_modified_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a
        ]
    


instance Data.Aeson.FromJSON UpdateDefaultConfigOutput where
    parseJSON = Data.Aeson.withObject "UpdateDefaultConfigOutput" $ \v -> UpdateDefaultConfigOutput
        Data.Functor.<$> (v Data.Aeson..: "key")
        Control.Applicative.<*> (v Data.Aeson..: "value")
        Control.Applicative.<*> (v Data.Aeson..: "schema")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "autocomplete_function_name")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
    



data UpdateDefaultConfigOutputBuilderState = UpdateDefaultConfigOutputBuilderState {
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    valueBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    schemaBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    autocomplete_function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateDefaultConfigOutputBuilderState
defaultBuilderState = UpdateDefaultConfigOutputBuilderState {
    keyBuilderState = Data.Maybe.Nothing,
    valueBuilderState = Data.Maybe.Nothing,
    schemaBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing,
    autocomplete_function_nameBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing
}

newtype UpdateDefaultConfigOutputBuilder a = UpdateDefaultConfigOutputBuilder {
    runUpdateDefaultConfigOutputBuilder :: UpdateDefaultConfigOutputBuilderState -> (UpdateDefaultConfigOutputBuilderState, a)
}

instance Data.Functor.Functor UpdateDefaultConfigOutputBuilder where
    fmap f (UpdateDefaultConfigOutputBuilder g) =
        UpdateDefaultConfigOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateDefaultConfigOutputBuilder where
    pure a = UpdateDefaultConfigOutputBuilder (\s -> (s, a))
    (UpdateDefaultConfigOutputBuilder f) <*> (UpdateDefaultConfigOutputBuilder g) = UpdateDefaultConfigOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateDefaultConfigOutputBuilder where
    (UpdateDefaultConfigOutputBuilder f) >>= g = UpdateDefaultConfigOutputBuilder (\s ->
        let (s', a) = f s
            (UpdateDefaultConfigOutputBuilder h) = g a
        in h s')

setKey :: Data.Text.Text -> UpdateDefaultConfigOutputBuilder ()
setKey value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { keyBuilderState = Data.Maybe.Just value }, ()))

setValue :: Data.Aeson.Value -> UpdateDefaultConfigOutputBuilder ()
setValue value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { valueBuilderState = Data.Maybe.Just value }, ()))

setSchema :: Data.Aeson.Value -> UpdateDefaultConfigOutputBuilder ()
setSchema value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { schemaBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> UpdateDefaultConfigOutputBuilder ()
setDescription value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> UpdateDefaultConfigOutputBuilder ()
setChangeReason value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setFunctionName :: Data.Maybe.Maybe Data.Text.Text -> UpdateDefaultConfigOutputBuilder ()
setFunctionName value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { function_nameBuilderState = value }, ()))

setAutocompleteFunctionName :: Data.Maybe.Maybe Data.Text.Text -> UpdateDefaultConfigOutputBuilder ()
setAutocompleteFunctionName value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { autocomplete_function_nameBuilderState = value }, ()))

setCreatedAt :: Data.Time.UTCTime -> UpdateDefaultConfigOutputBuilder ()
setCreatedAt value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setCreatedBy :: Data.Text.Text -> UpdateDefaultConfigOutputBuilder ()
setCreatedBy value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedAt :: Data.Time.UTCTime -> UpdateDefaultConfigOutputBuilder ()
setLastModifiedAt value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> UpdateDefaultConfigOutputBuilder ()
setLastModifiedBy value =
   UpdateDefaultConfigOutputBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

build :: UpdateDefaultConfigOutputBuilder () -> Data.Either.Either Data.Text.Text UpdateDefaultConfigOutput
build builder = do
    let (st, _) = runUpdateDefaultConfigOutputBuilder builder defaultBuilderState
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput.key is a required property.") Data.Either.Right (keyBuilderState st)
    value' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput.value is a required property.") Data.Either.Right (valueBuilderState st)
    schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput.schema is a required property.") Data.Either.Right (schemaBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    function_name' <- Data.Either.Right (function_nameBuilderState st)
    autocomplete_function_name' <- Data.Either.Right (autocomplete_function_nameBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    Data.Either.Right (UpdateDefaultConfigOutput { 
        key = key',
        value = value',
        schema = schema',
        description = description',
        change_reason = change_reason',
        function_name = function_name',
        autocomplete_function_name = autocomplete_function_name',
        created_at = created_at',
        created_by = created_by',
        last_modified_at = last_modified_at',
        last_modified_by = last_modified_by'
    })



module Io.Superposition.Model.CreateDefaultConfigOutput (
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
    CreateDefaultConfigOutputBuilder,
    CreateDefaultConfigOutput,
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
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data CreateDefaultConfigOutput = CreateDefaultConfigOutput {
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

instance Data.Aeson.ToJSON CreateDefaultConfigOutput where
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
    

instance Io.Superposition.Utility.SerializeBody CreateDefaultConfigOutput

instance Data.Aeson.FromJSON CreateDefaultConfigOutput where
    parseJSON = Data.Aeson.withObject "CreateDefaultConfigOutput" $ \v -> CreateDefaultConfigOutput
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
    



data CreateDefaultConfigOutputBuilderState = CreateDefaultConfigOutputBuilderState {
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

defaultBuilderState :: CreateDefaultConfigOutputBuilderState
defaultBuilderState = CreateDefaultConfigOutputBuilderState {
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

type CreateDefaultConfigOutputBuilder = Control.Monad.State.Strict.State CreateDefaultConfigOutputBuilderState

setKey :: Data.Text.Text -> CreateDefaultConfigOutputBuilder ()
setKey value =
   Control.Monad.State.Strict.modify (\s -> (s { keyBuilderState = Data.Maybe.Just value }))

setValue :: Data.Aeson.Value -> CreateDefaultConfigOutputBuilder ()
setValue value =
   Control.Monad.State.Strict.modify (\s -> (s { valueBuilderState = Data.Maybe.Just value }))

setSchema :: Data.Aeson.Value -> CreateDefaultConfigOutputBuilder ()
setSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { schemaBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> CreateDefaultConfigOutputBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> CreateDefaultConfigOutputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDefaultConfigOutputBuilder ()
setFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { function_nameBuilderState = value }))

setAutocompleteFunctionName :: Data.Maybe.Maybe Data.Text.Text -> CreateDefaultConfigOutputBuilder ()
setAutocompleteFunctionName value =
   Control.Monad.State.Strict.modify (\s -> (s { autocomplete_function_nameBuilderState = value }))

setCreatedAt :: Data.Time.UTCTime -> CreateDefaultConfigOutputBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> CreateDefaultConfigOutputBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> CreateDefaultConfigOutputBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> CreateDefaultConfigOutputBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

build :: CreateDefaultConfigOutputBuilder () -> Data.Either.Either Data.Text.Text CreateDefaultConfigOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigOutput.CreateDefaultConfigOutput.key is a required property.") Data.Either.Right (keyBuilderState st)
    value' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigOutput.CreateDefaultConfigOutput.value is a required property.") Data.Either.Right (valueBuilderState st)
    schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigOutput.CreateDefaultConfigOutput.schema is a required property.") Data.Either.Right (schemaBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigOutput.CreateDefaultConfigOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigOutput.CreateDefaultConfigOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    function_name' <- Data.Either.Right (function_nameBuilderState st)
    autocomplete_function_name' <- Data.Either.Right (autocomplete_function_nameBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigOutput.CreateDefaultConfigOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigOutput.CreateDefaultConfigOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigOutput.CreateDefaultConfigOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateDefaultConfigOutput.CreateDefaultConfigOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    Data.Either.Right (CreateDefaultConfigOutput { 
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


instance Io.Superposition.Utility.FromResponseParser CreateDefaultConfigOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "schema"
        var1 <- Io.Superposition.Utility.deSerField "autocomplete_function_name"
        var2 <- Io.Superposition.Utility.deSerField "change_reason"
        var3 <- Io.Superposition.Utility.deSerField "function_name"
        var4 <- Io.Superposition.Utility.deSerField "description"
        var5 <- Io.Superposition.Utility.deSerField "created_at"
        var6 <- Io.Superposition.Utility.deSerField "last_modified_by"
        var7 <- Io.Superposition.Utility.deSerField "value"
        var8 <- Io.Superposition.Utility.deSerField "created_by"
        var9 <- Io.Superposition.Utility.deSerField "key"
        var10 <- Io.Superposition.Utility.deSerField "last_modified_at"
        pure $ CreateDefaultConfigOutput {
            key = var9,
            value = var7,
            schema = var0,
            description = var4,
            change_reason = var2,
            function_name = var3,
            autocomplete_function_name = var1,
            created_at = var5,
            created_by = var8,
            last_modified_at = var10,
            last_modified_by = var6
        }


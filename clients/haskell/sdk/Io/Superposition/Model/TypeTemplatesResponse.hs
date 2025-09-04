module Io.Superposition.Model.TypeTemplatesResponse (
    setTypeName,
    setTypeSchema,
    setDescription,
    setChangeReason,
    setCreatedBy,
    setCreatedAt,
    setLastModifiedAt,
    setLastModifiedBy,
    build,
    TypeTemplatesResponseBuilder,
    TypeTemplatesResponse,
    type_name,
    type_schema,
    description,
    change_reason,
    created_by,
    created_at,
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

data TypeTemplatesResponse = TypeTemplatesResponse {
    type_name :: Data.Text.Text,
    type_schema :: Data.Aeson.Value,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    created_by :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    last_modified_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TypeTemplatesResponse where
    toJSON a = Data.Aeson.object [
        "type_name" Data.Aeson..= type_name a,
        "type_schema" Data.Aeson..= type_schema a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "created_by" Data.Aeson..= created_by a,
        "created_at" Data.Aeson..= created_at a,
        "last_modified_at" Data.Aeson..= last_modified_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a
        ]
    

instance Io.Superposition.Utility.SerializeBody TypeTemplatesResponse

instance Data.Aeson.FromJSON TypeTemplatesResponse where
    parseJSON = Data.Aeson.withObject "TypeTemplatesResponse" $ \v -> TypeTemplatesResponse
        Data.Functor.<$> (v Data.Aeson..: "type_name")
        Control.Applicative.<*> (v Data.Aeson..: "type_schema")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
    



data TypeTemplatesResponseBuilderState = TypeTemplatesResponseBuilderState {
    type_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    type_schemaBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TypeTemplatesResponseBuilderState
defaultBuilderState = TypeTemplatesResponseBuilderState {
    type_nameBuilderState = Data.Maybe.Nothing,
    type_schemaBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing
}

type TypeTemplatesResponseBuilder = Control.Monad.State.Strict.State TypeTemplatesResponseBuilderState

setTypeName :: Data.Text.Text -> TypeTemplatesResponseBuilder ()
setTypeName value =
   Control.Monad.State.Strict.modify (\s -> (s { type_nameBuilderState = Data.Maybe.Just value }))

setTypeSchema :: Data.Aeson.Value -> TypeTemplatesResponseBuilder ()
setTypeSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { type_schemaBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> TypeTemplatesResponseBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> TypeTemplatesResponseBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> TypeTemplatesResponseBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> TypeTemplatesResponseBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> TypeTemplatesResponseBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> TypeTemplatesResponseBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

build :: TypeTemplatesResponseBuilder () -> Data.Either.Either Data.Text.Text TypeTemplatesResponse
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    type_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse.type_name is a required property.") Data.Either.Right (type_nameBuilderState st)
    type_schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse.type_schema is a required property.") Data.Either.Right (type_schemaBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TypeTemplatesResponse.TypeTemplatesResponse.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    Data.Either.Right (TypeTemplatesResponse { 
        type_name = type_name',
        type_schema = type_schema',
        description = description',
        change_reason = change_reason',
        created_by = created_by',
        created_at = created_at',
        last_modified_at = last_modified_at',
        last_modified_by = last_modified_by'
    })



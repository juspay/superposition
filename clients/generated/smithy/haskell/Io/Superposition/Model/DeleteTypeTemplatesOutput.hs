module Io.Superposition.Model.DeleteTypeTemplatesOutput (
    setTypeName,
    setTypeSchema,
    setDescription,
    setChangeReason,
    setCreatedBy,
    setCreatedAt,
    setLastModifiedAt,
    setLastModifiedBy,
    build,
    DeleteTypeTemplatesOutputBuilder,
    DeleteTypeTemplatesOutput,
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

data DeleteTypeTemplatesOutput = DeleteTypeTemplatesOutput {
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

instance Data.Aeson.ToJSON DeleteTypeTemplatesOutput where
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
    


instance Data.Aeson.FromJSON DeleteTypeTemplatesOutput where
    parseJSON = Data.Aeson.withObject "DeleteTypeTemplatesOutput" $ \v -> DeleteTypeTemplatesOutput
        Data.Functor.<$> (v Data.Aeson..: "type_name")
        Control.Applicative.<*> (v Data.Aeson..: "type_schema")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
    



data DeleteTypeTemplatesOutputBuilderState = DeleteTypeTemplatesOutputBuilderState {
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

defaultBuilderState :: DeleteTypeTemplatesOutputBuilderState
defaultBuilderState = DeleteTypeTemplatesOutputBuilderState {
    type_nameBuilderState = Data.Maybe.Nothing,
    type_schemaBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing
}

newtype DeleteTypeTemplatesOutputBuilder a = DeleteTypeTemplatesOutputBuilder {
    runDeleteTypeTemplatesOutputBuilder :: DeleteTypeTemplatesOutputBuilderState -> (DeleteTypeTemplatesOutputBuilderState, a)
}

instance Data.Functor.Functor DeleteTypeTemplatesOutputBuilder where
    fmap f (DeleteTypeTemplatesOutputBuilder g) =
        DeleteTypeTemplatesOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DeleteTypeTemplatesOutputBuilder where
    pure a = DeleteTypeTemplatesOutputBuilder (\s -> (s, a))
    (DeleteTypeTemplatesOutputBuilder f) <*> (DeleteTypeTemplatesOutputBuilder g) = DeleteTypeTemplatesOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DeleteTypeTemplatesOutputBuilder where
    (DeleteTypeTemplatesOutputBuilder f) >>= g = DeleteTypeTemplatesOutputBuilder (\s ->
        let (s', a) = f s
            (DeleteTypeTemplatesOutputBuilder h) = g a
        in h s')

setTypeName :: Data.Text.Text -> DeleteTypeTemplatesOutputBuilder ()
setTypeName value =
   DeleteTypeTemplatesOutputBuilder (\s -> (s { type_nameBuilderState = Data.Maybe.Just value }, ()))

setTypeSchema :: Data.Aeson.Value -> DeleteTypeTemplatesOutputBuilder ()
setTypeSchema value =
   DeleteTypeTemplatesOutputBuilder (\s -> (s { type_schemaBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> DeleteTypeTemplatesOutputBuilder ()
setDescription value =
   DeleteTypeTemplatesOutputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> DeleteTypeTemplatesOutputBuilder ()
setChangeReason value =
   DeleteTypeTemplatesOutputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setCreatedBy :: Data.Text.Text -> DeleteTypeTemplatesOutputBuilder ()
setCreatedBy value =
   DeleteTypeTemplatesOutputBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setCreatedAt :: Data.Time.UTCTime -> DeleteTypeTemplatesOutputBuilder ()
setCreatedAt value =
   DeleteTypeTemplatesOutputBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedAt :: Data.Time.UTCTime -> DeleteTypeTemplatesOutputBuilder ()
setLastModifiedAt value =
   DeleteTypeTemplatesOutputBuilder (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> DeleteTypeTemplatesOutputBuilder ()
setLastModifiedBy value =
   DeleteTypeTemplatesOutputBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

build :: DeleteTypeTemplatesOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteTypeTemplatesOutput
build builder = do
    let (st, _) = runDeleteTypeTemplatesOutputBuilder builder defaultBuilderState
    type_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesOutput.DeleteTypeTemplatesOutput.type_name is a required property.") Data.Either.Right (type_nameBuilderState st)
    type_schema' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesOutput.DeleteTypeTemplatesOutput.type_schema is a required property.") Data.Either.Right (type_schemaBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesOutput.DeleteTypeTemplatesOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesOutput.DeleteTypeTemplatesOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesOutput.DeleteTypeTemplatesOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesOutput.DeleteTypeTemplatesOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesOutput.DeleteTypeTemplatesOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteTypeTemplatesOutput.DeleteTypeTemplatesOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    Data.Either.Right (DeleteTypeTemplatesOutput { 
        type_name = type_name',
        type_schema = type_schema',
        description = description',
        change_reason = change_reason',
        created_by = created_by',
        created_at = created_at',
        last_modified_at = last_modified_at',
        last_modified_by = last_modified_by'
    })



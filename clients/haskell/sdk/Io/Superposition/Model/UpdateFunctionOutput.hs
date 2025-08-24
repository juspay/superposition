module Io.Superposition.Model.UpdateFunctionOutput (
    setFunctionName,
    setPublishedCode,
    setDraftCode,
    setPublishedRuntimeVersion,
    setDraftRuntimeVersion,
    setPublishedAt,
    setDraftEditedAt,
    setPublishedBy,
    setDraftEditedBy,
    setLastModifiedAt,
    setLastModifiedBy,
    setChangeReason,
    setDescription,
    setFunctionType,
    build,
    UpdateFunctionOutputBuilder,
    UpdateFunctionOutput,
    function_name,
    published_code,
    draft_code,
    published_runtime_version,
    draft_runtime_version,
    published_at,
    draft_edited_at,
    published_by,
    draft_edited_by,
    last_modified_at,
    last_modified_by,
    change_reason,
    description,
    function_type
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
import qualified Io.Superposition.Model.FunctionTypes

data UpdateFunctionOutput = UpdateFunctionOutput {
    function_name :: Data.Text.Text,
    published_code :: Data.Maybe.Maybe Data.Text.Text,
    draft_code :: Data.Text.Text,
    published_runtime_version :: Data.Maybe.Maybe Data.Text.Text,
    draft_runtime_version :: Data.Text.Text,
    published_at :: Data.Maybe.Maybe Data.Time.UTCTime,
    draft_edited_at :: Data.Time.UTCTime,
    published_by :: Data.Maybe.Maybe Data.Text.Text,
    draft_edited_by :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    description :: Data.Text.Text,
    function_type :: Io.Superposition.Model.FunctionTypes.FunctionTypes
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateFunctionOutput where
    toJSON a = Data.Aeson.object [
        "function_name" Data.Aeson..= function_name a,
        "published_code" Data.Aeson..= published_code a,
        "draft_code" Data.Aeson..= draft_code a,
        "published_runtime_version" Data.Aeson..= published_runtime_version a,
        "draft_runtime_version" Data.Aeson..= draft_runtime_version a,
        "published_at" Data.Aeson..= published_at a,
        "draft_edited_at" Data.Aeson..= draft_edited_at a,
        "published_by" Data.Aeson..= published_by a,
        "draft_edited_by" Data.Aeson..= draft_edited_by a,
        "last_modified_at" Data.Aeson..= last_modified_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "change_reason" Data.Aeson..= change_reason a,
        "description" Data.Aeson..= description a,
        "function_type" Data.Aeson..= function_type a
        ]
    


instance Data.Aeson.FromJSON UpdateFunctionOutput where
    parseJSON = Data.Aeson.withObject "UpdateFunctionOutput" $ \v -> UpdateFunctionOutput
        Data.Functor.<$> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "published_code")
        Control.Applicative.<*> (v Data.Aeson..: "draft_code")
        Control.Applicative.<*> (v Data.Aeson..: "published_runtime_version")
        Control.Applicative.<*> (v Data.Aeson..: "draft_runtime_version")
        Control.Applicative.<*> (v Data.Aeson..: "published_at")
        Control.Applicative.<*> (v Data.Aeson..: "draft_edited_at")
        Control.Applicative.<*> (v Data.Aeson..: "published_by")
        Control.Applicative.<*> (v Data.Aeson..: "draft_edited_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "function_type")
    



data UpdateFunctionOutputBuilderState = UpdateFunctionOutputBuilderState {
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    published_codeBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    draft_codeBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    published_runtime_versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    draft_runtime_versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    published_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    draft_edited_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    published_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    draft_edited_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.FunctionTypes.FunctionTypes
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateFunctionOutputBuilderState
defaultBuilderState = UpdateFunctionOutputBuilderState {
    function_nameBuilderState = Data.Maybe.Nothing,
    published_codeBuilderState = Data.Maybe.Nothing,
    draft_codeBuilderState = Data.Maybe.Nothing,
    published_runtime_versionBuilderState = Data.Maybe.Nothing,
    draft_runtime_versionBuilderState = Data.Maybe.Nothing,
    published_atBuilderState = Data.Maybe.Nothing,
    draft_edited_atBuilderState = Data.Maybe.Nothing,
    published_byBuilderState = Data.Maybe.Nothing,
    draft_edited_byBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    function_typeBuilderState = Data.Maybe.Nothing
}

newtype UpdateFunctionOutputBuilder a = UpdateFunctionOutputBuilder {
    runUpdateFunctionOutputBuilder :: UpdateFunctionOutputBuilderState -> (UpdateFunctionOutputBuilderState, a)
}

instance Data.Functor.Functor UpdateFunctionOutputBuilder where
    fmap f (UpdateFunctionOutputBuilder g) =
        UpdateFunctionOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateFunctionOutputBuilder where
    pure a = UpdateFunctionOutputBuilder (\s -> (s, a))
    (UpdateFunctionOutputBuilder f) <*> (UpdateFunctionOutputBuilder g) = UpdateFunctionOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateFunctionOutputBuilder where
    (UpdateFunctionOutputBuilder f) >>= g = UpdateFunctionOutputBuilder (\s ->
        let (s', a) = f s
            (UpdateFunctionOutputBuilder h) = g a
        in h s')

setFunctionName :: Data.Text.Text -> UpdateFunctionOutputBuilder ()
setFunctionName value =
   UpdateFunctionOutputBuilder (\s -> (s { function_nameBuilderState = Data.Maybe.Just value }, ()))

setPublishedCode :: Data.Maybe.Maybe Data.Text.Text -> UpdateFunctionOutputBuilder ()
setPublishedCode value =
   UpdateFunctionOutputBuilder (\s -> (s { published_codeBuilderState = value }, ()))

setDraftCode :: Data.Text.Text -> UpdateFunctionOutputBuilder ()
setDraftCode value =
   UpdateFunctionOutputBuilder (\s -> (s { draft_codeBuilderState = Data.Maybe.Just value }, ()))

setPublishedRuntimeVersion :: Data.Maybe.Maybe Data.Text.Text -> UpdateFunctionOutputBuilder ()
setPublishedRuntimeVersion value =
   UpdateFunctionOutputBuilder (\s -> (s { published_runtime_versionBuilderState = value }, ()))

setDraftRuntimeVersion :: Data.Text.Text -> UpdateFunctionOutputBuilder ()
setDraftRuntimeVersion value =
   UpdateFunctionOutputBuilder (\s -> (s { draft_runtime_versionBuilderState = Data.Maybe.Just value }, ()))

setPublishedAt :: Data.Maybe.Maybe Data.Time.UTCTime -> UpdateFunctionOutputBuilder ()
setPublishedAt value =
   UpdateFunctionOutputBuilder (\s -> (s { published_atBuilderState = value }, ()))

setDraftEditedAt :: Data.Time.UTCTime -> UpdateFunctionOutputBuilder ()
setDraftEditedAt value =
   UpdateFunctionOutputBuilder (\s -> (s { draft_edited_atBuilderState = Data.Maybe.Just value }, ()))

setPublishedBy :: Data.Maybe.Maybe Data.Text.Text -> UpdateFunctionOutputBuilder ()
setPublishedBy value =
   UpdateFunctionOutputBuilder (\s -> (s { published_byBuilderState = value }, ()))

setDraftEditedBy :: Data.Text.Text -> UpdateFunctionOutputBuilder ()
setDraftEditedBy value =
   UpdateFunctionOutputBuilder (\s -> (s { draft_edited_byBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedAt :: Data.Time.UTCTime -> UpdateFunctionOutputBuilder ()
setLastModifiedAt value =
   UpdateFunctionOutputBuilder (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }, ()))

setLastModifiedBy :: Data.Text.Text -> UpdateFunctionOutputBuilder ()
setLastModifiedBy value =
   UpdateFunctionOutputBuilder (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> UpdateFunctionOutputBuilder ()
setChangeReason value =
   UpdateFunctionOutputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> UpdateFunctionOutputBuilder ()
setDescription value =
   UpdateFunctionOutputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setFunctionType :: Io.Superposition.Model.FunctionTypes.FunctionTypes -> UpdateFunctionOutputBuilder ()
setFunctionType value =
   UpdateFunctionOutputBuilder (\s -> (s { function_typeBuilderState = Data.Maybe.Just value }, ()))

build :: UpdateFunctionOutputBuilder () -> Data.Either.Either Data.Text.Text UpdateFunctionOutput
build builder = do
    let (st, _) = runUpdateFunctionOutputBuilder builder defaultBuilderState
    function_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionOutput.UpdateFunctionOutput.function_name is a required property.") Data.Either.Right (function_nameBuilderState st)
    published_code' <- Data.Either.Right (published_codeBuilderState st)
    draft_code' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionOutput.UpdateFunctionOutput.draft_code is a required property.") Data.Either.Right (draft_codeBuilderState st)
    published_runtime_version' <- Data.Either.Right (published_runtime_versionBuilderState st)
    draft_runtime_version' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionOutput.UpdateFunctionOutput.draft_runtime_version is a required property.") Data.Either.Right (draft_runtime_versionBuilderState st)
    published_at' <- Data.Either.Right (published_atBuilderState st)
    draft_edited_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionOutput.UpdateFunctionOutput.draft_edited_at is a required property.") Data.Either.Right (draft_edited_atBuilderState st)
    published_by' <- Data.Either.Right (published_byBuilderState st)
    draft_edited_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionOutput.UpdateFunctionOutput.draft_edited_by is a required property.") Data.Either.Right (draft_edited_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionOutput.UpdateFunctionOutput.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionOutput.UpdateFunctionOutput.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionOutput.UpdateFunctionOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionOutput.UpdateFunctionOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    function_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateFunctionOutput.UpdateFunctionOutput.function_type is a required property.") Data.Either.Right (function_typeBuilderState st)
    Data.Either.Right (UpdateFunctionOutput { 
        function_name = function_name',
        published_code = published_code',
        draft_code = draft_code',
        published_runtime_version = published_runtime_version',
        draft_runtime_version = draft_runtime_version',
        published_at = published_at',
        draft_edited_at = draft_edited_at',
        published_by = published_by',
        draft_edited_by = draft_edited_by',
        last_modified_at = last_modified_at',
        last_modified_by = last_modified_by',
        change_reason = change_reason',
        description = description',
        function_type = function_type'
    })



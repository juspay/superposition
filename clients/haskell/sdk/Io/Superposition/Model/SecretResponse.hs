module Io.Superposition.Model.SecretResponse (
    setName,
    setDescription,
    setChangeReason,
    setCreatedBy,
    setCreatedAt,
    setLastModifiedBy,
    setLastModifiedAt,
    build,
    SecretResponseBuilder,
    SecretResponse,
    name,
    description,
    change_reason,
    created_by,
    created_at,
    last_modified_by,
    last_modified_at
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

data SecretResponse = SecretResponse {
    name :: Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    created_by :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    last_modified_by :: Data.Text.Text,
    last_modified_at :: Data.Time.UTCTime
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON SecretResponse where
    toJSON a = Data.Aeson.object [
        "name" Data.Aeson..= name a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a,
        "created_by" Data.Aeson..= created_by a,
        "created_at" Data.Aeson..= created_at a,
        "last_modified_by" Data.Aeson..= last_modified_by a,
        "last_modified_at" Data.Aeson..= last_modified_at a
        ]
    

instance Io.Superposition.Utility.SerializeBody SecretResponse

instance Data.Aeson.FromJSON SecretResponse where
    parseJSON = Data.Aeson.withObject "SecretResponse" $ \v -> SecretResponse
        Data.Functor.<$> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_by")
        Control.Applicative.<*> (v Data.Aeson..: "last_modified_at")
    



data SecretResponseBuilderState = SecretResponseBuilderState {
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    last_modified_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    last_modified_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: SecretResponseBuilderState
defaultBuilderState = SecretResponseBuilderState {
    nameBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    last_modified_byBuilderState = Data.Maybe.Nothing,
    last_modified_atBuilderState = Data.Maybe.Nothing
}

type SecretResponseBuilder = Control.Monad.State.Strict.State SecretResponseBuilderState

setName :: Data.Text.Text -> SecretResponseBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Text.Text -> SecretResponseBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> SecretResponseBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setCreatedBy :: Data.Text.Text -> SecretResponseBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setCreatedAt :: Data.Time.UTCTime -> SecretResponseBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setLastModifiedBy :: Data.Text.Text -> SecretResponseBuilder ()
setLastModifiedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_byBuilderState = Data.Maybe.Just value }))

setLastModifiedAt :: Data.Time.UTCTime -> SecretResponseBuilder ()
setLastModifiedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modified_atBuilderState = Data.Maybe.Just value }))

build :: SecretResponseBuilder () -> Data.Either.Either Data.Text.Text SecretResponse
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.SecretResponse.SecretResponse.name is a required property.") Data.Either.Right (nameBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.SecretResponse.SecretResponse.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.SecretResponse.SecretResponse.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.SecretResponse.SecretResponse.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.SecretResponse.SecretResponse.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    last_modified_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.SecretResponse.SecretResponse.last_modified_by is a required property.") Data.Either.Right (last_modified_byBuilderState st)
    last_modified_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.SecretResponse.SecretResponse.last_modified_at is a required property.") Data.Either.Right (last_modified_atBuilderState st)
    Data.Either.Right (SecretResponse { 
        name = name',
        description = description',
        change_reason = change_reason',
        created_by = created_by',
        created_at = created_at',
        last_modified_by = last_modified_by',
        last_modified_at = last_modified_at'
    })



module Io.Superposition.Model.ListVersionsMember (
    setId',
    setConfig,
    setConfigHash,
    setCreatedAt,
    setDescription,
    setTags,
    build,
    ListVersionsMemberBuilder,
    ListVersionsMember,
    id',
    config,
    config_hash,
    created_at,
    description,
    tags
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

data ListVersionsMember = ListVersionsMember {
    id' :: Data.Text.Text,
    config :: Data.Aeson.Value,
    config_hash :: Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    description :: Data.Text.Text,
    tags :: Data.Maybe.Maybe ([] Data.Text.Text)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListVersionsMember where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "config" Data.Aeson..= config a,
        "config_hash" Data.Aeson..= config_hash a,
        "created_at" Data.Aeson..= created_at a,
        "description" Data.Aeson..= description a,
        "tags" Data.Aeson..= tags a
        ]
    


instance Data.Aeson.FromJSON ListVersionsMember where
    parseJSON = Data.Aeson.withObject "ListVersionsMember" $ \v -> ListVersionsMember
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "config")
        Control.Applicative.<*> (v Data.Aeson..: "config_hash")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "tags")
    



data ListVersionsMemberBuilderState = ListVersionsMemberBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    configBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    config_hashBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    tagsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListVersionsMemberBuilderState
defaultBuilderState = ListVersionsMemberBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    configBuilderState = Data.Maybe.Nothing,
    config_hashBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    tagsBuilderState = Data.Maybe.Nothing
}

newtype ListVersionsMemberBuilder a = ListVersionsMemberBuilder {
    runListVersionsMemberBuilder :: ListVersionsMemberBuilderState -> (ListVersionsMemberBuilderState, a)
}

instance Data.Functor.Functor ListVersionsMemberBuilder where
    fmap f (ListVersionsMemberBuilder g) =
        ListVersionsMemberBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListVersionsMemberBuilder where
    pure a = ListVersionsMemberBuilder (\s -> (s, a))
    (ListVersionsMemberBuilder f) <*> (ListVersionsMemberBuilder g) = ListVersionsMemberBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListVersionsMemberBuilder where
    (ListVersionsMemberBuilder f) >>= g = ListVersionsMemberBuilder (\s ->
        let (s', a) = f s
            (ListVersionsMemberBuilder h) = g a
        in h s')

setId' :: Data.Text.Text -> ListVersionsMemberBuilder ()
setId' value =
   ListVersionsMemberBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setConfig :: Data.Aeson.Value -> ListVersionsMemberBuilder ()
setConfig value =
   ListVersionsMemberBuilder (\s -> (s { configBuilderState = Data.Maybe.Just value }, ()))

setConfigHash :: Data.Text.Text -> ListVersionsMemberBuilder ()
setConfigHash value =
   ListVersionsMemberBuilder (\s -> (s { config_hashBuilderState = Data.Maybe.Just value }, ()))

setCreatedAt :: Data.Time.UTCTime -> ListVersionsMemberBuilder ()
setCreatedAt value =
   ListVersionsMemberBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> ListVersionsMemberBuilder ()
setDescription value =
   ListVersionsMemberBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setTags :: Data.Maybe.Maybe ([] Data.Text.Text) -> ListVersionsMemberBuilder ()
setTags value =
   ListVersionsMemberBuilder (\s -> (s { tagsBuilderState = value }, ()))

build :: ListVersionsMemberBuilder () -> Data.Either.Either Data.Text.Text ListVersionsMember
build builder = do
    let (st, _) = runListVersionsMemberBuilder builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVersionsMember.ListVersionsMember.id' is a required property.") Data.Either.Right (id'BuilderState st)
    config' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVersionsMember.ListVersionsMember.config is a required property.") Data.Either.Right (configBuilderState st)
    config_hash' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVersionsMember.ListVersionsMember.config_hash is a required property.") Data.Either.Right (config_hashBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVersionsMember.ListVersionsMember.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListVersionsMember.ListVersionsMember.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    tags' <- Data.Either.Right (tagsBuilderState st)
    Data.Either.Right (ListVersionsMember { 
        id' = id'',
        config = config',
        config_hash = config_hash',
        created_at = created_at',
        description = description',
        tags = tags'
    })



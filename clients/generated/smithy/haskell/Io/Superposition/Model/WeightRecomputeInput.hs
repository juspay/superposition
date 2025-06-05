module Io.Superposition.Model.WeightRecomputeInput (
    setWorkspaceId,
    setOrgId,
    setConfigTags,
    build,
    WeightRecomputeInputBuilder,
    WeightRecomputeInput,
    workspace_id,
    org_id,
    config_tags
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data WeightRecomputeInput = WeightRecomputeInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    config_tags :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WeightRecomputeInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "config_tags" Data.Aeson..= config_tags a
        ]
    


instance Data.Aeson.FromJSON WeightRecomputeInput where
    parseJSON = Data.Aeson.withObject "WeightRecomputeInput" $ \v -> WeightRecomputeInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "config_tags")
    



data WeightRecomputeInputBuilderState = WeightRecomputeInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    config_tagsBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WeightRecomputeInputBuilderState
defaultBuilderState = WeightRecomputeInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    config_tagsBuilderState = Data.Maybe.Nothing
}

newtype WeightRecomputeInputBuilder a = WeightRecomputeInputBuilder {
    runWeightRecomputeInputBuilder :: WeightRecomputeInputBuilderState -> (WeightRecomputeInputBuilderState, a)
}

instance Data.Functor.Functor WeightRecomputeInputBuilder where
    fmap f (WeightRecomputeInputBuilder g) =
        WeightRecomputeInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative WeightRecomputeInputBuilder where
    pure a = WeightRecomputeInputBuilder (\s -> (s, a))
    (WeightRecomputeInputBuilder f) <*> (WeightRecomputeInputBuilder g) = WeightRecomputeInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad WeightRecomputeInputBuilder where
    (WeightRecomputeInputBuilder f) >>= g = WeightRecomputeInputBuilder (\s ->
        let (s', a) = f s
            (WeightRecomputeInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> WeightRecomputeInputBuilder ()
setWorkspaceId value =
   WeightRecomputeInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> WeightRecomputeInputBuilder ()
setOrgId value =
   WeightRecomputeInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setConfigTags :: Data.Maybe.Maybe Data.Text.Text -> WeightRecomputeInputBuilder ()
setConfigTags value =
   WeightRecomputeInputBuilder (\s -> (s { config_tagsBuilderState = value }, ()))

build :: WeightRecomputeInputBuilder () -> Data.Either.Either Data.Text.Text WeightRecomputeInput
build builder = do
    let (st, _) = runWeightRecomputeInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WeightRecomputeInput.WeightRecomputeInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WeightRecomputeInput.WeightRecomputeInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    config_tags' <- Data.Either.Right (config_tagsBuilderState st)
    Data.Either.Right (WeightRecomputeInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        config_tags = config_tags'
    })



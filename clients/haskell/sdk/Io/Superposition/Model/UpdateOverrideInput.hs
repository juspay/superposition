module Io.Superposition.Model.UpdateOverrideInput (
    setWorkspaceId,
    setOrgId,
    setConfigTags,
    setRequest,
    build,
    UpdateOverrideInputBuilder,
    UpdateOverrideInput,
    workspace_id,
    org_id,
    config_tags,
    request
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
import qualified Io.Superposition.Model.UpdateContextOverrideRequest

data UpdateOverrideInput = UpdateOverrideInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    config_tags :: Data.Maybe.Maybe Data.Text.Text,
    request :: Io.Superposition.Model.UpdateContextOverrideRequest.UpdateContextOverrideRequest
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateOverrideInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "config_tags" Data.Aeson..= config_tags a,
        "request" Data.Aeson..= request a
        ]
    


instance Data.Aeson.FromJSON UpdateOverrideInput where
    parseJSON = Data.Aeson.withObject "UpdateOverrideInput" $ \v -> UpdateOverrideInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "config_tags")
        Control.Applicative.<*> (v Data.Aeson..: "request")
    



data UpdateOverrideInputBuilderState = UpdateOverrideInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    config_tagsBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    requestBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.UpdateContextOverrideRequest.UpdateContextOverrideRequest
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateOverrideInputBuilderState
defaultBuilderState = UpdateOverrideInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    config_tagsBuilderState = Data.Maybe.Nothing,
    requestBuilderState = Data.Maybe.Nothing
}

newtype UpdateOverrideInputBuilder a = UpdateOverrideInputBuilder {
    runUpdateOverrideInputBuilder :: UpdateOverrideInputBuilderState -> (UpdateOverrideInputBuilderState, a)
}

instance Data.Functor.Functor UpdateOverrideInputBuilder where
    fmap f (UpdateOverrideInputBuilder g) =
        UpdateOverrideInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UpdateOverrideInputBuilder where
    pure a = UpdateOverrideInputBuilder (\s -> (s, a))
    (UpdateOverrideInputBuilder f) <*> (UpdateOverrideInputBuilder g) = UpdateOverrideInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UpdateOverrideInputBuilder where
    (UpdateOverrideInputBuilder f) >>= g = UpdateOverrideInputBuilder (\s ->
        let (s', a) = f s
            (UpdateOverrideInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> UpdateOverrideInputBuilder ()
setWorkspaceId value =
   UpdateOverrideInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> UpdateOverrideInputBuilder ()
setOrgId value =
   UpdateOverrideInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setConfigTags :: Data.Maybe.Maybe Data.Text.Text -> UpdateOverrideInputBuilder ()
setConfigTags value =
   UpdateOverrideInputBuilder (\s -> (s { config_tagsBuilderState = value }, ()))

setRequest :: Io.Superposition.Model.UpdateContextOverrideRequest.UpdateContextOverrideRequest -> UpdateOverrideInputBuilder ()
setRequest value =
   UpdateOverrideInputBuilder (\s -> (s { requestBuilderState = Data.Maybe.Just value }, ()))

build :: UpdateOverrideInputBuilder () -> Data.Either.Either Data.Text.Text UpdateOverrideInput
build builder = do
    let (st, _) = runUpdateOverrideInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideInput.UpdateOverrideInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideInput.UpdateOverrideInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    config_tags' <- Data.Either.Right (config_tagsBuilderState st)
    request' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOverrideInput.UpdateOverrideInput.request is a required property.") Data.Either.Right (requestBuilderState st)
    Data.Either.Right (UpdateOverrideInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        config_tags = config_tags',
        request = request'
    })



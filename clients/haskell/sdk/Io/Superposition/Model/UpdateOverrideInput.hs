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
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.UpdateContextOverrideRequest
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

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
    

instance Io.Superposition.Utility.SerializeBody UpdateOverrideInput

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

type UpdateOverrideInputBuilder = Control.Monad.State.Strict.State UpdateOverrideInputBuilderState

setWorkspaceId :: Data.Text.Text -> UpdateOverrideInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> UpdateOverrideInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setConfigTags :: Data.Maybe.Maybe Data.Text.Text -> UpdateOverrideInputBuilder ()
setConfigTags value =
   Control.Monad.State.Strict.modify (\s -> (s { config_tagsBuilderState = value }))

setRequest :: Io.Superposition.Model.UpdateContextOverrideRequest.UpdateContextOverrideRequest -> UpdateOverrideInputBuilder ()
setRequest value =
   Control.Monad.State.Strict.modify (\s -> (s { requestBuilderState = Data.Maybe.Just value }))

build :: UpdateOverrideInputBuilder () -> Data.Either.Either Data.Text.Text UpdateOverrideInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
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


instance Io.Superposition.Utility.IntoRequestBuilder UpdateOverrideInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPut
        Io.Superposition.Utility.setPath [
            "context",
            "overrides"
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serHeader "x-config-tags" (config_tags self)
        Io.Superposition.Utility.serBody "application/json" (request self)


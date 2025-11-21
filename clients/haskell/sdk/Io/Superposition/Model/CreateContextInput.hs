module Io.Superposition.Model.CreateContextInput (
    setWorkspaceId,
    setOrgId,
    setConfigTags,
    setRequest,
    build,
    CreateContextInputBuilder,
    CreateContextInput,
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
import qualified Io.Superposition.Model.ContextPut
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data CreateContextInput = CreateContextInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    config_tags :: Data.Maybe.Maybe Data.Text.Text,
    request :: Io.Superposition.Model.ContextPut.ContextPut
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateContextInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "config_tags" Data.Aeson..= config_tags a,
        "request" Data.Aeson..= request a
        ]
    

instance Io.Superposition.Utility.SerializeBody CreateContextInput

instance Data.Aeson.FromJSON CreateContextInput where
    parseJSON = Data.Aeson.withObject "CreateContextInput" $ \v -> CreateContextInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "config_tags")
        Control.Applicative.<*> (v Data.Aeson..: "request")
    



data CreateContextInputBuilderState = CreateContextInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    config_tagsBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    requestBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ContextPut.ContextPut
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateContextInputBuilderState
defaultBuilderState = CreateContextInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    config_tagsBuilderState = Data.Maybe.Nothing,
    requestBuilderState = Data.Maybe.Nothing
}

type CreateContextInputBuilder = Control.Monad.State.Strict.State CreateContextInputBuilderState

setWorkspaceId :: Data.Text.Text -> CreateContextInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> CreateContextInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setConfigTags :: Data.Maybe.Maybe Data.Text.Text -> CreateContextInputBuilder ()
setConfigTags value =
   Control.Monad.State.Strict.modify (\s -> (s { config_tagsBuilderState = value }))

setRequest :: Io.Superposition.Model.ContextPut.ContextPut -> CreateContextInputBuilder ()
setRequest value =
   Control.Monad.State.Strict.modify (\s -> (s { requestBuilderState = Data.Maybe.Just value }))

build :: CreateContextInputBuilder () -> Data.Either.Either Data.Text.Text CreateContextInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextInput.CreateContextInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextInput.CreateContextInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    config_tags' <- Data.Either.Right (config_tagsBuilderState st)
    request' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextInput.CreateContextInput.request is a required property.") Data.Either.Right (requestBuilderState st)
    Data.Either.Right (CreateContextInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        config_tags = config_tags',
        request = request'
    })


instance Io.Superposition.Utility.IntoRequestBuilder CreateContextInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPut
        Io.Superposition.Utility.setPath [
            "context"
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serHeader "x-config-tags" (config_tags self)
        Io.Superposition.Utility.serBody "application/json" (request self)


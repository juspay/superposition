module Io.Superposition.Model.GetDefaultConfigInput (
    setWorkspaceId,
    setOrgId,
    setKey,
    build,
    GetDefaultConfigInputBuilder,
    GetDefaultConfigInput,
    workspace_id,
    org_id,
    key
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
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data GetDefaultConfigInput = GetDefaultConfigInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    key :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetDefaultConfigInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "key" Data.Aeson..= key a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetDefaultConfigInput

instance Data.Aeson.FromJSON GetDefaultConfigInput where
    parseJSON = Data.Aeson.withObject "GetDefaultConfigInput" $ \v -> GetDefaultConfigInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "key")
    



data GetDefaultConfigInputBuilderState = GetDefaultConfigInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetDefaultConfigInputBuilderState
defaultBuilderState = GetDefaultConfigInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    keyBuilderState = Data.Maybe.Nothing
}

type GetDefaultConfigInputBuilder = Control.Monad.State.Strict.State GetDefaultConfigInputBuilderState

setWorkspaceId :: Data.Text.Text -> GetDefaultConfigInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> GetDefaultConfigInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setKey :: Data.Text.Text -> GetDefaultConfigInputBuilder ()
setKey value =
   Control.Monad.State.Strict.modify (\s -> (s { keyBuilderState = Data.Maybe.Just value }))

build :: GetDefaultConfigInputBuilder () -> Data.Either.Either Data.Text.Text GetDefaultConfigInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDefaultConfigInput.GetDefaultConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDefaultConfigInput.GetDefaultConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDefaultConfigInput.GetDefaultConfigInput.key is a required property.") Data.Either.Right (keyBuilderState st)
    Data.Either.Right (GetDefaultConfigInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        key = key'
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetDefaultConfigInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "default-config",
            Io.Superposition.Utility.serializeElement (key self)
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


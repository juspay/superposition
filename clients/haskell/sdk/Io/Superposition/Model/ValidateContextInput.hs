module Io.Superposition.Model.ValidateContextInput (
    setWorkspaceId,
    setOrgId,
    setContext,
    build,
    ValidateContextInputBuilder,
    ValidateContextInput,
    workspace_id,
    org_id,
    context
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data ValidateContextInput = ValidateContextInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ValidateContextInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "context" Data.Aeson..= context a
        ]
    

instance Io.Superposition.Utility.SerializeBody ValidateContextInput

instance Data.Aeson.FromJSON ValidateContextInput where
    parseJSON = Data.Aeson.withObject "ValidateContextInput" $ \v -> ValidateContextInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "context")
    



data ValidateContextInputBuilderState = ValidateContextInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ValidateContextInputBuilderState
defaultBuilderState = ValidateContextInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing
}

type ValidateContextInputBuilder = Control.Monad.State.Strict.State ValidateContextInputBuilderState

setWorkspaceId :: Data.Text.Text -> ValidateContextInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ValidateContextInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> ValidateContextInputBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = Data.Maybe.Just value }))

build :: ValidateContextInputBuilder () -> Data.Either.Either Data.Text.Text ValidateContextInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValidateContextInput.ValidateContextInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValidateContextInput.ValidateContextInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ValidateContextInput.ValidateContextInput.context is a required property.") Data.Either.Right (contextBuilderState st)
    Data.Either.Right (ValidateContextInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        context = context'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ValidateContextInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPut
        Io.Superposition.Utility.setPath [
            "context",
            "validate"
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "context" (context self)


module Io.Superposition.Model.DeleteVariableInput (
    setWorkspaceId,
    setOrgId,
    setName,
    build,
    DeleteVariableInputBuilder,
    DeleteVariableInput,
    workspace_id,
    org_id,
    name
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

data DeleteVariableInput = DeleteVariableInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteVariableInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "name" Data.Aeson..= name a
        ]
    

instance Io.Superposition.Utility.SerializeBody DeleteVariableInput

instance Data.Aeson.FromJSON DeleteVariableInput where
    parseJSON = Data.Aeson.withObject "DeleteVariableInput" $ \v -> DeleteVariableInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "name")
    



data DeleteVariableInputBuilderState = DeleteVariableInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteVariableInputBuilderState
defaultBuilderState = DeleteVariableInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing
}

type DeleteVariableInputBuilder = Control.Monad.State.Strict.State DeleteVariableInputBuilderState

setWorkspaceId :: Data.Text.Text -> DeleteVariableInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> DeleteVariableInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> DeleteVariableInputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

build :: DeleteVariableInputBuilder () -> Data.Either.Either Data.Text.Text DeleteVariableInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteVariableInput.DeleteVariableInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteVariableInput.DeleteVariableInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteVariableInput.DeleteVariableInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    Data.Either.Right (DeleteVariableInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        name = name'
    })


instance Io.Superposition.Utility.IntoRequestBuilder DeleteVariableInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodDelete
        Io.Superposition.Utility.setPath [
            "variables",
            Io.Superposition.Utility.serializeElement (name self)
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        


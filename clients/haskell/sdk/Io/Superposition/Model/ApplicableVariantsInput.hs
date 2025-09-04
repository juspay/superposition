module Io.Superposition.Model.ApplicableVariantsInput (
    setWorkspaceId,
    setOrgId,
    setContext,
    setIdentifier,
    build,
    ApplicableVariantsInputBuilder,
    ApplicableVariantsInput,
    workspace_id,
    org_id,
    context,
    identifier
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

data ApplicableVariantsInput = ApplicableVariantsInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    identifier :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ApplicableVariantsInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "context" Data.Aeson..= context a,
        "identifier" Data.Aeson..= identifier a
        ]
    

instance Io.Superposition.Utility.SerializeBody ApplicableVariantsInput

instance Data.Aeson.FromJSON ApplicableVariantsInput where
    parseJSON = Data.Aeson.withObject "ApplicableVariantsInput" $ \v -> ApplicableVariantsInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "identifier")
    



data ApplicableVariantsInputBuilderState = ApplicableVariantsInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    identifierBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ApplicableVariantsInputBuilderState
defaultBuilderState = ApplicableVariantsInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing,
    identifierBuilderState = Data.Maybe.Nothing
}

type ApplicableVariantsInputBuilder = Control.Monad.State.Strict.State ApplicableVariantsInputBuilderState

setWorkspaceId :: Data.Text.Text -> ApplicableVariantsInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> ApplicableVariantsInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> ApplicableVariantsInputBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = Data.Maybe.Just value }))

setIdentifier :: Data.Text.Text -> ApplicableVariantsInputBuilder ()
setIdentifier value =
   Control.Monad.State.Strict.modify (\s -> (s { identifierBuilderState = Data.Maybe.Just value }))

build :: ApplicableVariantsInputBuilder () -> Data.Either.Either Data.Text.Text ApplicableVariantsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput.context is a required property.") Data.Either.Right (contextBuilderState st)
    identifier' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput.identifier is a required property.") Data.Either.Right (identifierBuilderState st)
    Data.Either.Right (ApplicableVariantsInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        context = context',
        identifier = identifier'
    })


instance Io.Superposition.Utility.IntoRequestBuilder ApplicableVariantsInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "experiments",
            "applicable-variants"
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "identifier" (identifier self)
        Io.Superposition.Utility.serField "context" (context self)


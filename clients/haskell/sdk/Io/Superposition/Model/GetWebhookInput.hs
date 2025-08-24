module Io.Superposition.Model.GetWebhookInput (
    setWorkspaceId,
    setOrgId,
    setName,
    build,
    GetWebhookInputBuilder,
    GetWebhookInput,
    workspace_id,
    org_id,
    name
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

data GetWebhookInput = GetWebhookInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetWebhookInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "name" Data.Aeson..= name a
        ]
    


instance Data.Aeson.FromJSON GetWebhookInput where
    parseJSON = Data.Aeson.withObject "GetWebhookInput" $ \v -> GetWebhookInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "name")
    



data GetWebhookInputBuilderState = GetWebhookInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetWebhookInputBuilderState
defaultBuilderState = GetWebhookInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing
}

newtype GetWebhookInputBuilder a = GetWebhookInputBuilder {
    runGetWebhookInputBuilder :: GetWebhookInputBuilderState -> (GetWebhookInputBuilderState, a)
}

instance Data.Functor.Functor GetWebhookInputBuilder where
    fmap f (GetWebhookInputBuilder g) =
        GetWebhookInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetWebhookInputBuilder where
    pure a = GetWebhookInputBuilder (\s -> (s, a))
    (GetWebhookInputBuilder f) <*> (GetWebhookInputBuilder g) = GetWebhookInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetWebhookInputBuilder where
    (GetWebhookInputBuilder f) >>= g = GetWebhookInputBuilder (\s ->
        let (s', a) = f s
            (GetWebhookInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> GetWebhookInputBuilder ()
setWorkspaceId value =
   GetWebhookInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> GetWebhookInputBuilder ()
setOrgId value =
   GetWebhookInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setName :: Data.Text.Text -> GetWebhookInputBuilder ()
setName value =
   GetWebhookInputBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

build :: GetWebhookInputBuilder () -> Data.Either.Either Data.Text.Text GetWebhookInput
build builder = do
    let (st, _) = runGetWebhookInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookInput.GetWebhookInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookInput.GetWebhookInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetWebhookInput.GetWebhookInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    Data.Either.Right (GetWebhookInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        name = name'
    })



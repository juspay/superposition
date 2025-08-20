module Io.Superposition.Model.GetConfigInput (
    setWorkspaceId,
    setOrgId,
    setPrefix,
    setVersion,
    setContext,
    build,
    GetConfigInputBuilder,
    GetConfigInput,
    workspace_id,
    org_id,
    prefix,
    version,
    context
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data GetConfigInput = GetConfigInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    prefix :: Data.Maybe.Maybe Data.Text.Text,
    version :: Data.Maybe.Maybe Data.Text.Text,
    context :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetConfigInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "prefix" Data.Aeson..= prefix a,
        "version" Data.Aeson..= version a,
        "context" Data.Aeson..= context a
        ]
    


instance Data.Aeson.FromJSON GetConfigInput where
    parseJSON = Data.Aeson.withObject "GetConfigInput" $ \v -> GetConfigInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "prefix")
        Control.Applicative.<*> (v Data.Aeson..: "version")
        Control.Applicative.<*> (v Data.Aeson..: "context")
    



data GetConfigInputBuilderState = GetConfigInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    prefixBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    versionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetConfigInputBuilderState
defaultBuilderState = GetConfigInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    prefixBuilderState = Data.Maybe.Nothing,
    versionBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing
}

newtype GetConfigInputBuilder a = GetConfigInputBuilder {
    runGetConfigInputBuilder :: GetConfigInputBuilderState -> (GetConfigInputBuilderState, a)
}

instance Data.Functor.Functor GetConfigInputBuilder where
    fmap f (GetConfigInputBuilder g) =
        GetConfigInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetConfigInputBuilder where
    pure a = GetConfigInputBuilder (\s -> (s, a))
    (GetConfigInputBuilder f) <*> (GetConfigInputBuilder g) = GetConfigInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetConfigInputBuilder where
    (GetConfigInputBuilder f) >>= g = GetConfigInputBuilder (\s ->
        let (s', a) = f s
            (GetConfigInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> GetConfigInputBuilder ()
setWorkspaceId value =
   GetConfigInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> GetConfigInputBuilder ()
setOrgId value =
   GetConfigInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setPrefix :: Data.Maybe.Maybe Data.Text.Text -> GetConfigInputBuilder ()
setPrefix value =
   GetConfigInputBuilder (\s -> (s { prefixBuilderState = value }, ()))

setVersion :: Data.Maybe.Maybe Data.Text.Text -> GetConfigInputBuilder ()
setVersion value =
   GetConfigInputBuilder (\s -> (s { versionBuilderState = value }, ()))

setContext :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> GetConfigInputBuilder ()
setContext value =
   GetConfigInputBuilder (\s -> (s { contextBuilderState = value }, ()))

build :: GetConfigInputBuilder () -> Data.Either.Either Data.Text.Text GetConfigInput
build builder = do
    let (st, _) = runGetConfigInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetConfigInput.GetConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetConfigInput.GetConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    prefix' <- Data.Either.Right (prefixBuilderState st)
    version' <- Data.Either.Right (versionBuilderState st)
    context' <- Data.Either.Right (contextBuilderState st)
    Data.Either.Right (GetConfigInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        prefix = prefix',
        version = version',
        context = context'
    })



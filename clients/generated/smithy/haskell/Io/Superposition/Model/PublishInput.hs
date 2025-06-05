module Io.Superposition.Model.PublishInput (
    setWorkspaceId,
    setOrgId,
    setFunctionName,
    build,
    PublishInputBuilder,
    PublishInput,
    workspace_id,
    org_id,
    function_name
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

data PublishInput = PublishInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    function_name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON PublishInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "function_name" Data.Aeson..= function_name a
        ]
    


instance Data.Aeson.FromJSON PublishInput where
    parseJSON = Data.Aeson.withObject "PublishInput" $ \v -> PublishInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
    



data PublishInputBuilderState = PublishInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: PublishInputBuilderState
defaultBuilderState = PublishInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing
}

newtype PublishInputBuilder a = PublishInputBuilder {
    runPublishInputBuilder :: PublishInputBuilderState -> (PublishInputBuilderState, a)
}

instance Data.Functor.Functor PublishInputBuilder where
    fmap f (PublishInputBuilder g) =
        PublishInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative PublishInputBuilder where
    pure a = PublishInputBuilder (\s -> (s, a))
    (PublishInputBuilder f) <*> (PublishInputBuilder g) = PublishInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad PublishInputBuilder where
    (PublishInputBuilder f) >>= g = PublishInputBuilder (\s ->
        let (s', a) = f s
            (PublishInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> PublishInputBuilder ()
setWorkspaceId value =
   PublishInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> PublishInputBuilder ()
setOrgId value =
   PublishInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setFunctionName :: Data.Text.Text -> PublishInputBuilder ()
setFunctionName value =
   PublishInputBuilder (\s -> (s { function_nameBuilderState = Data.Maybe.Just value }, ()))

build :: PublishInputBuilder () -> Data.Either.Either Data.Text.Text PublishInput
build builder = do
    let (st, _) = runPublishInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.PublishInput.PublishInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.PublishInput.PublishInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    function_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.PublishInput.PublishInput.function_name is a required property.") Data.Either.Right (function_nameBuilderState st)
    Data.Either.Right (PublishInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        function_name = function_name'
    })



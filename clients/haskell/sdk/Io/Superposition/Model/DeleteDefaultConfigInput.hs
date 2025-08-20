module Io.Superposition.Model.DeleteDefaultConfigInput (
    setWorkspaceId,
    setOrgId,
    setKey,
    build,
    DeleteDefaultConfigInputBuilder,
    DeleteDefaultConfigInput,
    workspace_id,
    org_id,
    key
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

data DeleteDefaultConfigInput = DeleteDefaultConfigInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    key :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteDefaultConfigInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "key" Data.Aeson..= key a
        ]
    


instance Data.Aeson.FromJSON DeleteDefaultConfigInput where
    parseJSON = Data.Aeson.withObject "DeleteDefaultConfigInput" $ \v -> DeleteDefaultConfigInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "key")
    



data DeleteDefaultConfigInputBuilderState = DeleteDefaultConfigInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteDefaultConfigInputBuilderState
defaultBuilderState = DeleteDefaultConfigInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    keyBuilderState = Data.Maybe.Nothing
}

newtype DeleteDefaultConfigInputBuilder a = DeleteDefaultConfigInputBuilder {
    runDeleteDefaultConfigInputBuilder :: DeleteDefaultConfigInputBuilderState -> (DeleteDefaultConfigInputBuilderState, a)
}

instance Data.Functor.Functor DeleteDefaultConfigInputBuilder where
    fmap f (DeleteDefaultConfigInputBuilder g) =
        DeleteDefaultConfigInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DeleteDefaultConfigInputBuilder where
    pure a = DeleteDefaultConfigInputBuilder (\s -> (s, a))
    (DeleteDefaultConfigInputBuilder f) <*> (DeleteDefaultConfigInputBuilder g) = DeleteDefaultConfigInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DeleteDefaultConfigInputBuilder where
    (DeleteDefaultConfigInputBuilder f) >>= g = DeleteDefaultConfigInputBuilder (\s ->
        let (s', a) = f s
            (DeleteDefaultConfigInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> DeleteDefaultConfigInputBuilder ()
setWorkspaceId value =
   DeleteDefaultConfigInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> DeleteDefaultConfigInputBuilder ()
setOrgId value =
   DeleteDefaultConfigInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setKey :: Data.Text.Text -> DeleteDefaultConfigInputBuilder ()
setKey value =
   DeleteDefaultConfigInputBuilder (\s -> (s { keyBuilderState = Data.Maybe.Just value }, ()))

build :: DeleteDefaultConfigInputBuilder () -> Data.Either.Either Data.Text.Text DeleteDefaultConfigInput
build builder = do
    let (st, _) = runDeleteDefaultConfigInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteDefaultConfigInput.DeleteDefaultConfigInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteDefaultConfigInput.DeleteDefaultConfigInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteDefaultConfigInput.DeleteDefaultConfigInput.key is a required property.") Data.Either.Right (keyBuilderState st)
    Data.Either.Right (DeleteDefaultConfigInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        key = key'
    })



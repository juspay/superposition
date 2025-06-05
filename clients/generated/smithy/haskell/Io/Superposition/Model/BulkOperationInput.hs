module Io.Superposition.Model.BulkOperationInput (
    setWorkspaceId,
    setOrgId,
    setConfigTags,
    setBulkOperation,
    build,
    BulkOperationInputBuilder,
    BulkOperationInput,
    workspace_id,
    org_id,
    config_tags,
    bulk_operation
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
import qualified Io.Superposition.Model.BulkOperationReq

data BulkOperationInput = BulkOperationInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    config_tags :: Data.Maybe.Maybe Data.Text.Text,
    bulk_operation :: Io.Superposition.Model.BulkOperationReq.BulkOperationReq
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON BulkOperationInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "config_tags" Data.Aeson..= config_tags a,
        "bulk_operation" Data.Aeson..= bulk_operation a
        ]
    


instance Data.Aeson.FromJSON BulkOperationInput where
    parseJSON = Data.Aeson.withObject "BulkOperationInput" $ \v -> BulkOperationInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "config_tags")
        Control.Applicative.<*> (v Data.Aeson..: "bulk_operation")
    



data BulkOperationInputBuilderState = BulkOperationInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    config_tagsBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    bulk_operationBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.BulkOperationReq.BulkOperationReq
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: BulkOperationInputBuilderState
defaultBuilderState = BulkOperationInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    config_tagsBuilderState = Data.Maybe.Nothing,
    bulk_operationBuilderState = Data.Maybe.Nothing
}

newtype BulkOperationInputBuilder a = BulkOperationInputBuilder {
    runBulkOperationInputBuilder :: BulkOperationInputBuilderState -> (BulkOperationInputBuilderState, a)
}

instance Data.Functor.Functor BulkOperationInputBuilder where
    fmap f (BulkOperationInputBuilder g) =
        BulkOperationInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative BulkOperationInputBuilder where
    pure a = BulkOperationInputBuilder (\s -> (s, a))
    (BulkOperationInputBuilder f) <*> (BulkOperationInputBuilder g) = BulkOperationInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad BulkOperationInputBuilder where
    (BulkOperationInputBuilder f) >>= g = BulkOperationInputBuilder (\s ->
        let (s', a) = f s
            (BulkOperationInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> BulkOperationInputBuilder ()
setWorkspaceId value =
   BulkOperationInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> BulkOperationInputBuilder ()
setOrgId value =
   BulkOperationInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setConfigTags :: Data.Maybe.Maybe Data.Text.Text -> BulkOperationInputBuilder ()
setConfigTags value =
   BulkOperationInputBuilder (\s -> (s { config_tagsBuilderState = value }, ()))

setBulkOperation :: Io.Superposition.Model.BulkOperationReq.BulkOperationReq -> BulkOperationInputBuilder ()
setBulkOperation value =
   BulkOperationInputBuilder (\s -> (s { bulk_operationBuilderState = Data.Maybe.Just value }, ()))

build :: BulkOperationInputBuilder () -> Data.Either.Either Data.Text.Text BulkOperationInput
build builder = do
    let (st, _) = runBulkOperationInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.BulkOperationInput.BulkOperationInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.BulkOperationInput.BulkOperationInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    config_tags' <- Data.Either.Right (config_tagsBuilderState st)
    bulk_operation' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.BulkOperationInput.BulkOperationInput.bulk_operation is a required property.") Data.Either.Right (bulk_operationBuilderState st)
    Data.Either.Right (BulkOperationInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        config_tags = config_tags',
        bulk_operation = bulk_operation'
    })



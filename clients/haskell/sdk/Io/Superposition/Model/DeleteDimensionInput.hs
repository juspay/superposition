module Io.Superposition.Model.DeleteDimensionInput (
    setWorkspaceId,
    setOrgId,
    setDimension,
    build,
    DeleteDimensionInputBuilder,
    DeleteDimensionInput,
    workspace_id,
    org_id,
    dimension
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

data DeleteDimensionInput = DeleteDimensionInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    dimension :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteDimensionInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "dimension" Data.Aeson..= dimension a
        ]
    


instance Data.Aeson.FromJSON DeleteDimensionInput where
    parseJSON = Data.Aeson.withObject "DeleteDimensionInput" $ \v -> DeleteDimensionInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "dimension")
    



data DeleteDimensionInputBuilderState = DeleteDimensionInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimensionBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteDimensionInputBuilderState
defaultBuilderState = DeleteDimensionInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    dimensionBuilderState = Data.Maybe.Nothing
}

newtype DeleteDimensionInputBuilder a = DeleteDimensionInputBuilder {
    runDeleteDimensionInputBuilder :: DeleteDimensionInputBuilderState -> (DeleteDimensionInputBuilderState, a)
}

instance Data.Functor.Functor DeleteDimensionInputBuilder where
    fmap f (DeleteDimensionInputBuilder g) =
        DeleteDimensionInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DeleteDimensionInputBuilder where
    pure a = DeleteDimensionInputBuilder (\s -> (s, a))
    (DeleteDimensionInputBuilder f) <*> (DeleteDimensionInputBuilder g) = DeleteDimensionInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DeleteDimensionInputBuilder where
    (DeleteDimensionInputBuilder f) >>= g = DeleteDimensionInputBuilder (\s ->
        let (s', a) = f s
            (DeleteDimensionInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> DeleteDimensionInputBuilder ()
setWorkspaceId value =
   DeleteDimensionInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> DeleteDimensionInputBuilder ()
setOrgId value =
   DeleteDimensionInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setDimension :: Data.Text.Text -> DeleteDimensionInputBuilder ()
setDimension value =
   DeleteDimensionInputBuilder (\s -> (s { dimensionBuilderState = Data.Maybe.Just value }, ()))

build :: DeleteDimensionInputBuilder () -> Data.Either.Either Data.Text.Text DeleteDimensionInput
build builder = do
    let (st, _) = runDeleteDimensionInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteDimensionInput.DeleteDimensionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteDimensionInput.DeleteDimensionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    dimension' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DeleteDimensionInput.DeleteDimensionInput.dimension is a required property.") Data.Either.Right (dimensionBuilderState st)
    Data.Either.Right (DeleteDimensionInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        dimension = dimension'
    })



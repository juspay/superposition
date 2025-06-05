module Io.Superposition.Model.GetDimensionInput (
    setWorkspaceId,
    setOrgId,
    setDimension,
    build,
    GetDimensionInputBuilder,
    GetDimensionInput,
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

data GetDimensionInput = GetDimensionInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    dimension :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetDimensionInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "dimension" Data.Aeson..= dimension a
        ]
    


instance Data.Aeson.FromJSON GetDimensionInput where
    parseJSON = Data.Aeson.withObject "GetDimensionInput" $ \v -> GetDimensionInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "dimension")
    



data GetDimensionInputBuilderState = GetDimensionInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    dimensionBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetDimensionInputBuilderState
defaultBuilderState = GetDimensionInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    dimensionBuilderState = Data.Maybe.Nothing
}

newtype GetDimensionInputBuilder a = GetDimensionInputBuilder {
    runGetDimensionInputBuilder :: GetDimensionInputBuilderState -> (GetDimensionInputBuilderState, a)
}

instance Data.Functor.Functor GetDimensionInputBuilder where
    fmap f (GetDimensionInputBuilder g) =
        GetDimensionInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetDimensionInputBuilder where
    pure a = GetDimensionInputBuilder (\s -> (s, a))
    (GetDimensionInputBuilder f) <*> (GetDimensionInputBuilder g) = GetDimensionInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetDimensionInputBuilder where
    (GetDimensionInputBuilder f) >>= g = GetDimensionInputBuilder (\s ->
        let (s', a) = f s
            (GetDimensionInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> GetDimensionInputBuilder ()
setWorkspaceId value =
   GetDimensionInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> GetDimensionInputBuilder ()
setOrgId value =
   GetDimensionInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setDimension :: Data.Text.Text -> GetDimensionInputBuilder ()
setDimension value =
   GetDimensionInputBuilder (\s -> (s { dimensionBuilderState = Data.Maybe.Just value }, ()))

build :: GetDimensionInputBuilder () -> Data.Either.Either Data.Text.Text GetDimensionInput
build builder = do
    let (st, _) = runGetDimensionInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDimensionInput.GetDimensionInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDimensionInput.GetDimensionInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    dimension' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetDimensionInput.GetDimensionInput.dimension is a required property.") Data.Either.Right (dimensionBuilderState st)
    Data.Either.Right (GetDimensionInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        dimension = dimension'
    })



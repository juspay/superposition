module Io.Superposition.Model.GetExperimentGroupInput (
    setWorkspaceId,
    setOrgId,
    setId',
    build,
    GetExperimentGroupInputBuilder,
    GetExperimentGroupInput,
    workspace_id,
    org_id,
    id'
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

data GetExperimentGroupInput = GetExperimentGroupInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetExperimentGroupInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a
        ]
    


instance Data.Aeson.FromJSON GetExperimentGroupInput where
    parseJSON = Data.Aeson.withObject "GetExperimentGroupInput" $ \v -> GetExperimentGroupInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
    



data GetExperimentGroupInputBuilderState = GetExperimentGroupInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetExperimentGroupInputBuilderState
defaultBuilderState = GetExperimentGroupInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing
}

newtype GetExperimentGroupInputBuilder a = GetExperimentGroupInputBuilder {
    runGetExperimentGroupInputBuilder :: GetExperimentGroupInputBuilderState -> (GetExperimentGroupInputBuilderState, a)
}

instance Data.Functor.Functor GetExperimentGroupInputBuilder where
    fmap f (GetExperimentGroupInputBuilder g) =
        GetExperimentGroupInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetExperimentGroupInputBuilder where
    pure a = GetExperimentGroupInputBuilder (\s -> (s, a))
    (GetExperimentGroupInputBuilder f) <*> (GetExperimentGroupInputBuilder g) = GetExperimentGroupInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetExperimentGroupInputBuilder where
    (GetExperimentGroupInputBuilder f) >>= g = GetExperimentGroupInputBuilder (\s ->
        let (s', a) = f s
            (GetExperimentGroupInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> GetExperimentGroupInputBuilder ()
setWorkspaceId value =
   GetExperimentGroupInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> GetExperimentGroupInputBuilder ()
setOrgId value =
   GetExperimentGroupInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setId' :: Data.Text.Text -> GetExperimentGroupInputBuilder ()
setId' value =
   GetExperimentGroupInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

build :: GetExperimentGroupInputBuilder () -> Data.Either.Either Data.Text.Text GetExperimentGroupInput
build builder = do
    let (st, _) = runGetExperimentGroupInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetExperimentGroupInput.GetExperimentGroupInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetExperimentGroupInput.GetExperimentGroupInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetExperimentGroupInput.GetExperimentGroupInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    Data.Either.Right (GetExperimentGroupInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id''
    })



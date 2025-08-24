module Io.Superposition.Model.PauseExperimentInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setChangeReason,
    build,
    PauseExperimentInputBuilder,
    PauseExperimentInput,
    workspace_id,
    org_id,
    id',
    change_reason
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

data PauseExperimentInput = PauseExperimentInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON PauseExperimentInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON PauseExperimentInput where
    parseJSON = Data.Aeson.withObject "PauseExperimentInput" $ \v -> PauseExperimentInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data PauseExperimentInputBuilderState = PauseExperimentInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: PauseExperimentInputBuilderState
defaultBuilderState = PauseExperimentInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype PauseExperimentInputBuilder a = PauseExperimentInputBuilder {
    runPauseExperimentInputBuilder :: PauseExperimentInputBuilderState -> (PauseExperimentInputBuilderState, a)
}

instance Data.Functor.Functor PauseExperimentInputBuilder where
    fmap f (PauseExperimentInputBuilder g) =
        PauseExperimentInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative PauseExperimentInputBuilder where
    pure a = PauseExperimentInputBuilder (\s -> (s, a))
    (PauseExperimentInputBuilder f) <*> (PauseExperimentInputBuilder g) = PauseExperimentInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad PauseExperimentInputBuilder where
    (PauseExperimentInputBuilder f) >>= g = PauseExperimentInputBuilder (\s ->
        let (s', a) = f s
            (PauseExperimentInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> PauseExperimentInputBuilder ()
setWorkspaceId value =
   PauseExperimentInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> PauseExperimentInputBuilder ()
setOrgId value =
   PauseExperimentInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setId' :: Data.Text.Text -> PauseExperimentInputBuilder ()
setId' value =
   PauseExperimentInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> PauseExperimentInputBuilder ()
setChangeReason value =
   PauseExperimentInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: PauseExperimentInputBuilder () -> Data.Either.Either Data.Text.Text PauseExperimentInput
build builder = do
    let (st, _) = runPauseExperimentInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.PauseExperimentInput.PauseExperimentInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.PauseExperimentInput.PauseExperimentInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.PauseExperimentInput.PauseExperimentInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.PauseExperimentInput.PauseExperimentInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (PauseExperimentInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        change_reason = change_reason'
    })



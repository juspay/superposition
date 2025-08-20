module Io.Superposition.Model.ResumeExperimentInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setChangeReason,
    build,
    ResumeExperimentInputBuilder,
    ResumeExperimentInput,
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

data ResumeExperimentInput = ResumeExperimentInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ResumeExperimentInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON ResumeExperimentInput where
    parseJSON = Data.Aeson.withObject "ResumeExperimentInput" $ \v -> ResumeExperimentInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data ResumeExperimentInputBuilderState = ResumeExperimentInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ResumeExperimentInputBuilderState
defaultBuilderState = ResumeExperimentInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype ResumeExperimentInputBuilder a = ResumeExperimentInputBuilder {
    runResumeExperimentInputBuilder :: ResumeExperimentInputBuilderState -> (ResumeExperimentInputBuilderState, a)
}

instance Data.Functor.Functor ResumeExperimentInputBuilder where
    fmap f (ResumeExperimentInputBuilder g) =
        ResumeExperimentInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ResumeExperimentInputBuilder where
    pure a = ResumeExperimentInputBuilder (\s -> (s, a))
    (ResumeExperimentInputBuilder f) <*> (ResumeExperimentInputBuilder g) = ResumeExperimentInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ResumeExperimentInputBuilder where
    (ResumeExperimentInputBuilder f) >>= g = ResumeExperimentInputBuilder (\s ->
        let (s', a) = f s
            (ResumeExperimentInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> ResumeExperimentInputBuilder ()
setWorkspaceId value =
   ResumeExperimentInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> ResumeExperimentInputBuilder ()
setOrgId value =
   ResumeExperimentInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setId' :: Data.Text.Text -> ResumeExperimentInputBuilder ()
setId' value =
   ResumeExperimentInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> ResumeExperimentInputBuilder ()
setChangeReason value =
   ResumeExperimentInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: ResumeExperimentInputBuilder () -> Data.Either.Either Data.Text.Text ResumeExperimentInput
build builder = do
    let (st, _) = runResumeExperimentInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResumeExperimentInput.ResumeExperimentInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResumeExperimentInput.ResumeExperimentInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResumeExperimentInput.ResumeExperimentInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResumeExperimentInput.ResumeExperimentInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (ResumeExperimentInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        change_reason = change_reason'
    })



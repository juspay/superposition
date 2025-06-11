module Io.Superposition.Model.DiscardExperimentInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setChangeReason,
    build,
    DiscardExperimentInputBuilder,
    DiscardExperimentInput,
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

data DiscardExperimentInput = DiscardExperimentInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DiscardExperimentInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON DiscardExperimentInput where
    parseJSON = Data.Aeson.withObject "DiscardExperimentInput" $ \v -> DiscardExperimentInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data DiscardExperimentInputBuilderState = DiscardExperimentInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DiscardExperimentInputBuilderState
defaultBuilderState = DiscardExperimentInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype DiscardExperimentInputBuilder a = DiscardExperimentInputBuilder {
    runDiscardExperimentInputBuilder :: DiscardExperimentInputBuilderState -> (DiscardExperimentInputBuilderState, a)
}

instance Data.Functor.Functor DiscardExperimentInputBuilder where
    fmap f (DiscardExperimentInputBuilder g) =
        DiscardExperimentInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DiscardExperimentInputBuilder where
    pure a = DiscardExperimentInputBuilder (\s -> (s, a))
    (DiscardExperimentInputBuilder f) <*> (DiscardExperimentInputBuilder g) = DiscardExperimentInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DiscardExperimentInputBuilder where
    (DiscardExperimentInputBuilder f) >>= g = DiscardExperimentInputBuilder (\s ->
        let (s', a) = f s
            (DiscardExperimentInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> DiscardExperimentInputBuilder ()
setWorkspaceId value =
   DiscardExperimentInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> DiscardExperimentInputBuilder ()
setOrgId value =
   DiscardExperimentInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setId' :: Data.Text.Text -> DiscardExperimentInputBuilder ()
setId' value =
   DiscardExperimentInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> DiscardExperimentInputBuilder ()
setChangeReason value =
   DiscardExperimentInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: DiscardExperimentInputBuilder () -> Data.Either.Either Data.Text.Text DiscardExperimentInput
build builder = do
    let (st, _) = runDiscardExperimentInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DiscardExperimentInput.DiscardExperimentInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DiscardExperimentInput.DiscardExperimentInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DiscardExperimentInput.DiscardExperimentInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.DiscardExperimentInput.DiscardExperimentInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (DiscardExperimentInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        change_reason = change_reason'
    })



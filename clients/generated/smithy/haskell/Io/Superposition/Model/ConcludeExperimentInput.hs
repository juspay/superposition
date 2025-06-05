module Io.Superposition.Model.ConcludeExperimentInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setChosenVariant,
    setDescription,
    setChangeReason,
    build,
    ConcludeExperimentInputBuilder,
    ConcludeExperimentInput,
    workspace_id,
    org_id,
    id',
    chosen_variant,
    description,
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

data ConcludeExperimentInput = ConcludeExperimentInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    chosen_variant :: Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ConcludeExperimentInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "chosen_variant" Data.Aeson..= chosen_variant a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON ConcludeExperimentInput where
    parseJSON = Data.Aeson.withObject "ConcludeExperimentInput" $ \v -> ConcludeExperimentInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "chosen_variant")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data ConcludeExperimentInputBuilderState = ConcludeExperimentInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    chosen_variantBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ConcludeExperimentInputBuilderState
defaultBuilderState = ConcludeExperimentInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    chosen_variantBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype ConcludeExperimentInputBuilder a = ConcludeExperimentInputBuilder {
    runConcludeExperimentInputBuilder :: ConcludeExperimentInputBuilderState -> (ConcludeExperimentInputBuilderState, a)
}

instance Data.Functor.Functor ConcludeExperimentInputBuilder where
    fmap f (ConcludeExperimentInputBuilder g) =
        ConcludeExperimentInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ConcludeExperimentInputBuilder where
    pure a = ConcludeExperimentInputBuilder (\s -> (s, a))
    (ConcludeExperimentInputBuilder f) <*> (ConcludeExperimentInputBuilder g) = ConcludeExperimentInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ConcludeExperimentInputBuilder where
    (ConcludeExperimentInputBuilder f) >>= g = ConcludeExperimentInputBuilder (\s ->
        let (s', a) = f s
            (ConcludeExperimentInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> ConcludeExperimentInputBuilder ()
setWorkspaceId value =
   ConcludeExperimentInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> ConcludeExperimentInputBuilder ()
setOrgId value =
   ConcludeExperimentInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setId' :: Data.Text.Text -> ConcludeExperimentInputBuilder ()
setId' value =
   ConcludeExperimentInputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setChosenVariant :: Data.Text.Text -> ConcludeExperimentInputBuilder ()
setChosenVariant value =
   ConcludeExperimentInputBuilder (\s -> (s { chosen_variantBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> ConcludeExperimentInputBuilder ()
setDescription value =
   ConcludeExperimentInputBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setChangeReason :: Data.Text.Text -> ConcludeExperimentInputBuilder ()
setChangeReason value =
   ConcludeExperimentInputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: ConcludeExperimentInputBuilder () -> Data.Either.Either Data.Text.Text ConcludeExperimentInput
build builder = do
    let (st, _) = runConcludeExperimentInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ConcludeExperimentInput.ConcludeExperimentInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ConcludeExperimentInput.ConcludeExperimentInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ConcludeExperimentInput.ConcludeExperimentInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    chosen_variant' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ConcludeExperimentInput.ConcludeExperimentInput.chosen_variant is a required property.") Data.Either.Right (chosen_variantBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ConcludeExperimentInput.ConcludeExperimentInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (ConcludeExperimentInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        chosen_variant = chosen_variant',
        description = description',
        change_reason = change_reason'
    })



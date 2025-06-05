module Io.Superposition.Model.ApplicableVariantsInput (
    setWorkspaceId,
    setOrgId,
    setContext,
    setToss,
    build,
    ApplicableVariantsInputBuilder,
    ApplicableVariantsInput,
    workspace_id,
    org_id,
    context,
    toss
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data ApplicableVariantsInput = ApplicableVariantsInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    toss :: Integer
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ApplicableVariantsInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "context" Data.Aeson..= context a,
        "toss" Data.Aeson..= toss a
        ]
    


instance Data.Aeson.FromJSON ApplicableVariantsInput where
    parseJSON = Data.Aeson.withObject "ApplicableVariantsInput" $ \v -> ApplicableVariantsInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "toss")
    



data ApplicableVariantsInputBuilderState = ApplicableVariantsInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    tossBuilderState :: Data.Maybe.Maybe Integer
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ApplicableVariantsInputBuilderState
defaultBuilderState = ApplicableVariantsInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing,
    tossBuilderState = Data.Maybe.Nothing
}

newtype ApplicableVariantsInputBuilder a = ApplicableVariantsInputBuilder {
    runApplicableVariantsInputBuilder :: ApplicableVariantsInputBuilderState -> (ApplicableVariantsInputBuilderState, a)
}

instance Data.Functor.Functor ApplicableVariantsInputBuilder where
    fmap f (ApplicableVariantsInputBuilder g) =
        ApplicableVariantsInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ApplicableVariantsInputBuilder where
    pure a = ApplicableVariantsInputBuilder (\s -> (s, a))
    (ApplicableVariantsInputBuilder f) <*> (ApplicableVariantsInputBuilder g) = ApplicableVariantsInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ApplicableVariantsInputBuilder where
    (ApplicableVariantsInputBuilder f) >>= g = ApplicableVariantsInputBuilder (\s ->
        let (s', a) = f s
            (ApplicableVariantsInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> ApplicableVariantsInputBuilder ()
setWorkspaceId value =
   ApplicableVariantsInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> ApplicableVariantsInputBuilder ()
setOrgId value =
   ApplicableVariantsInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> ApplicableVariantsInputBuilder ()
setContext value =
   ApplicableVariantsInputBuilder (\s -> (s { contextBuilderState = Data.Maybe.Just value }, ()))

setToss :: Integer -> ApplicableVariantsInputBuilder ()
setToss value =
   ApplicableVariantsInputBuilder (\s -> (s { tossBuilderState = Data.Maybe.Just value }, ()))

build :: ApplicableVariantsInputBuilder () -> Data.Either.Either Data.Text.Text ApplicableVariantsInput
build builder = do
    let (st, _) = runApplicableVariantsInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput.context is a required property.") Data.Either.Right (contextBuilderState st)
    toss' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput.toss is a required property.") Data.Either.Right (tossBuilderState st)
    Data.Either.Right (ApplicableVariantsInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        context = context',
        toss = toss'
    })



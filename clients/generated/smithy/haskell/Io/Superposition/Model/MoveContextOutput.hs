module Io.Superposition.Model.MoveContextOutput (
    setContextId,
    setOverrideId,
    setWeight,
    setDescription,
    setChangeReason,
    build,
    MoveContextOutputBuilder,
    MoveContextOutput,
    context_id,
    override_id,
    weight,
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

data MoveContextOutput = MoveContextOutput {
    context_id :: Data.Text.Text,
    override_id :: Data.Text.Text,
    weight :: Data.Text.Text,
    description :: Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON MoveContextOutput where
    toJSON a = Data.Aeson.object [
        "context_id" Data.Aeson..= context_id a,
        "override_id" Data.Aeson..= override_id a,
        "weight" Data.Aeson..= weight a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON MoveContextOutput where
    parseJSON = Data.Aeson.withObject "MoveContextOutput" $ \v -> MoveContextOutput
        Data.Functor.<$> (v Data.Aeson..: "context_id")
        Control.Applicative.<*> (v Data.Aeson..: "override_id")
        Control.Applicative.<*> (v Data.Aeson..: "weight")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data MoveContextOutputBuilderState = MoveContextOutputBuilderState {
    context_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    override_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    weightBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: MoveContextOutputBuilderState
defaultBuilderState = MoveContextOutputBuilderState {
    context_idBuilderState = Data.Maybe.Nothing,
    override_idBuilderState = Data.Maybe.Nothing,
    weightBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype MoveContextOutputBuilder a = MoveContextOutputBuilder {
    runMoveContextOutputBuilder :: MoveContextOutputBuilderState -> (MoveContextOutputBuilderState, a)
}

instance Data.Functor.Functor MoveContextOutputBuilder where
    fmap f (MoveContextOutputBuilder g) =
        MoveContextOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative MoveContextOutputBuilder where
    pure a = MoveContextOutputBuilder (\s -> (s, a))
    (MoveContextOutputBuilder f) <*> (MoveContextOutputBuilder g) = MoveContextOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad MoveContextOutputBuilder where
    (MoveContextOutputBuilder f) >>= g = MoveContextOutputBuilder (\s ->
        let (s', a) = f s
            (MoveContextOutputBuilder h) = g a
        in h s')

setContextId :: Data.Text.Text -> MoveContextOutputBuilder ()
setContextId value =
   MoveContextOutputBuilder (\s -> (s { context_idBuilderState = Data.Maybe.Just value }, ()))

setOverrideId :: Data.Text.Text -> MoveContextOutputBuilder ()
setOverrideId value =
   MoveContextOutputBuilder (\s -> (s { override_idBuilderState = Data.Maybe.Just value }, ()))

setWeight :: Data.Text.Text -> MoveContextOutputBuilder ()
setWeight value =
   MoveContextOutputBuilder (\s -> (s { weightBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> MoveContextOutputBuilder ()
setDescription value =
   MoveContextOutputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> MoveContextOutputBuilder ()
setChangeReason value =
   MoveContextOutputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: MoveContextOutputBuilder () -> Data.Either.Either Data.Text.Text MoveContextOutput
build builder = do
    let (st, _) = runMoveContextOutputBuilder builder defaultBuilderState
    context_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextOutput.MoveContextOutput.context_id is a required property.") Data.Either.Right (context_idBuilderState st)
    override_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextOutput.MoveContextOutput.override_id is a required property.") Data.Either.Right (override_idBuilderState st)
    weight' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextOutput.MoveContextOutput.weight is a required property.") Data.Either.Right (weightBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextOutput.MoveContextOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.MoveContextOutput.MoveContextOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (MoveContextOutput { 
        context_id = context_id',
        override_id = override_id',
        weight = weight',
        description = description',
        change_reason = change_reason'
    })



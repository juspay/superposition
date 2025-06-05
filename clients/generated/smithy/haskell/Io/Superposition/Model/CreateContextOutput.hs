module Io.Superposition.Model.CreateContextOutput (
    setContextId,
    setOverrideId,
    setWeight,
    setDescription,
    setChangeReason,
    build,
    CreateContextOutputBuilder,
    CreateContextOutput,
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

data CreateContextOutput = CreateContextOutput {
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

instance Data.Aeson.ToJSON CreateContextOutput where
    toJSON a = Data.Aeson.object [
        "context_id" Data.Aeson..= context_id a,
        "override_id" Data.Aeson..= override_id a,
        "weight" Data.Aeson..= weight a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON CreateContextOutput where
    parseJSON = Data.Aeson.withObject "CreateContextOutput" $ \v -> CreateContextOutput
        Data.Functor.<$> (v Data.Aeson..: "context_id")
        Control.Applicative.<*> (v Data.Aeson..: "override_id")
        Control.Applicative.<*> (v Data.Aeson..: "weight")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data CreateContextOutputBuilderState = CreateContextOutputBuilderState {
    context_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    override_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    weightBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateContextOutputBuilderState
defaultBuilderState = CreateContextOutputBuilderState {
    context_idBuilderState = Data.Maybe.Nothing,
    override_idBuilderState = Data.Maybe.Nothing,
    weightBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype CreateContextOutputBuilder a = CreateContextOutputBuilder {
    runCreateContextOutputBuilder :: CreateContextOutputBuilderState -> (CreateContextOutputBuilderState, a)
}

instance Data.Functor.Functor CreateContextOutputBuilder where
    fmap f (CreateContextOutputBuilder g) =
        CreateContextOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CreateContextOutputBuilder where
    pure a = CreateContextOutputBuilder (\s -> (s, a))
    (CreateContextOutputBuilder f) <*> (CreateContextOutputBuilder g) = CreateContextOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CreateContextOutputBuilder where
    (CreateContextOutputBuilder f) >>= g = CreateContextOutputBuilder (\s ->
        let (s', a) = f s
            (CreateContextOutputBuilder h) = g a
        in h s')

setContextId :: Data.Text.Text -> CreateContextOutputBuilder ()
setContextId value =
   CreateContextOutputBuilder (\s -> (s { context_idBuilderState = Data.Maybe.Just value }, ()))

setOverrideId :: Data.Text.Text -> CreateContextOutputBuilder ()
setOverrideId value =
   CreateContextOutputBuilder (\s -> (s { override_idBuilderState = Data.Maybe.Just value }, ()))

setWeight :: Data.Text.Text -> CreateContextOutputBuilder ()
setWeight value =
   CreateContextOutputBuilder (\s -> (s { weightBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Text.Text -> CreateContextOutputBuilder ()
setDescription value =
   CreateContextOutputBuilder (\s -> (s { descriptionBuilderState = Data.Maybe.Just value }, ()))

setChangeReason :: Data.Text.Text -> CreateContextOutputBuilder ()
setChangeReason value =
   CreateContextOutputBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: CreateContextOutputBuilder () -> Data.Either.Either Data.Text.Text CreateContextOutput
build builder = do
    let (st, _) = runCreateContextOutputBuilder builder defaultBuilderState
    context_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextOutput.CreateContextOutput.context_id is a required property.") Data.Either.Right (context_idBuilderState st)
    override_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextOutput.CreateContextOutput.override_id is a required property.") Data.Either.Right (override_idBuilderState st)
    weight' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextOutput.CreateContextOutput.weight is a required property.") Data.Either.Right (weightBuilderState st)
    description' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextOutput.CreateContextOutput.description is a required property.") Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateContextOutput.CreateContextOutput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (CreateContextOutput { 
        context_id = context_id',
        override_id = override_id',
        weight = weight',
        description = description',
        change_reason = change_reason'
    })



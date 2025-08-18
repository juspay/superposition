module Io.Superposition.Model.ContextMoveOut (
    setContextId,
    setOverrideId,
    setWeight,
    setDescription,
    setChangeReason,
    build,
    ContextMoveOutBuilder,
    ContextMoveOut,
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

data ContextMoveOut = ContextMoveOut {
    context_id :: Data.Maybe.Maybe Data.Text.Text,
    override_id :: Data.Maybe.Maybe Data.Text.Text,
    weight :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ContextMoveOut where
    toJSON a = Data.Aeson.object [
        "context_id" Data.Aeson..= context_id a,
        "override_id" Data.Aeson..= override_id a,
        "weight" Data.Aeson..= weight a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON ContextMoveOut where
    parseJSON = Data.Aeson.withObject "ContextMoveOut" $ \v -> ContextMoveOut
        Data.Functor.<$> (v Data.Aeson..: "context_id")
        Control.Applicative.<*> (v Data.Aeson..: "override_id")
        Control.Applicative.<*> (v Data.Aeson..: "weight")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data ContextMoveOutBuilderState = ContextMoveOutBuilderState {
    context_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    override_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    weightBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ContextMoveOutBuilderState
defaultBuilderState = ContextMoveOutBuilderState {
    context_idBuilderState = Data.Maybe.Nothing,
    override_idBuilderState = Data.Maybe.Nothing,
    weightBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype ContextMoveOutBuilder a = ContextMoveOutBuilder {
    runContextMoveOutBuilder :: ContextMoveOutBuilderState -> (ContextMoveOutBuilderState, a)
}

instance Data.Functor.Functor ContextMoveOutBuilder where
    fmap f (ContextMoveOutBuilder g) =
        ContextMoveOutBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ContextMoveOutBuilder where
    pure a = ContextMoveOutBuilder (\s -> (s, a))
    (ContextMoveOutBuilder f) <*> (ContextMoveOutBuilder g) = ContextMoveOutBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ContextMoveOutBuilder where
    (ContextMoveOutBuilder f) >>= g = ContextMoveOutBuilder (\s ->
        let (s', a) = f s
            (ContextMoveOutBuilder h) = g a
        in h s')

setContextId :: Data.Maybe.Maybe Data.Text.Text -> ContextMoveOutBuilder ()
setContextId value =
   ContextMoveOutBuilder (\s -> (s { context_idBuilderState = value }, ()))

setOverrideId :: Data.Maybe.Maybe Data.Text.Text -> ContextMoveOutBuilder ()
setOverrideId value =
   ContextMoveOutBuilder (\s -> (s { override_idBuilderState = value }, ()))

setWeight :: Data.Maybe.Maybe Data.Text.Text -> ContextMoveOutBuilder ()
setWeight value =
   ContextMoveOutBuilder (\s -> (s { weightBuilderState = value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> ContextMoveOutBuilder ()
setDescription value =
   ContextMoveOutBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setChangeReason :: Data.Maybe.Maybe Data.Text.Text -> ContextMoveOutBuilder ()
setChangeReason value =
   ContextMoveOutBuilder (\s -> (s { change_reasonBuilderState = value }, ()))

build :: ContextMoveOutBuilder () -> Data.Either.Either Data.Text.Text ContextMoveOut
build builder = do
    let (st, _) = runContextMoveOutBuilder builder defaultBuilderState
    context_id' <- Data.Either.Right (context_idBuilderState st)
    override_id' <- Data.Either.Right (override_idBuilderState st)
    weight' <- Data.Either.Right (weightBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (ContextMoveOut { 
        context_id = context_id',
        override_id = override_id',
        weight = weight',
        description = description',
        change_reason = change_reason'
    })



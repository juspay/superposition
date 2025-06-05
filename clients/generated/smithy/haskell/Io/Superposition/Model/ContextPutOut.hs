module Io.Superposition.Model.ContextPutOut (
    setContextId,
    setOverrideId,
    setWeight,
    setDescription,
    setChangeReason,
    build,
    ContextPutOutBuilder,
    ContextPutOut,
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

data ContextPutOut = ContextPutOut {
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

instance Data.Aeson.ToJSON ContextPutOut where
    toJSON a = Data.Aeson.object [
        "context_id" Data.Aeson..= context_id a,
        "override_id" Data.Aeson..= override_id a,
        "weight" Data.Aeson..= weight a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON ContextPutOut where
    parseJSON = Data.Aeson.withObject "ContextPutOut" $ \v -> ContextPutOut
        Data.Functor.<$> (v Data.Aeson..: "context_id")
        Control.Applicative.<*> (v Data.Aeson..: "override_id")
        Control.Applicative.<*> (v Data.Aeson..: "weight")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data ContextPutOutBuilderState = ContextPutOutBuilderState {
    context_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    override_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    weightBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ContextPutOutBuilderState
defaultBuilderState = ContextPutOutBuilderState {
    context_idBuilderState = Data.Maybe.Nothing,
    override_idBuilderState = Data.Maybe.Nothing,
    weightBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype ContextPutOutBuilder a = ContextPutOutBuilder {
    runContextPutOutBuilder :: ContextPutOutBuilderState -> (ContextPutOutBuilderState, a)
}

instance Data.Functor.Functor ContextPutOutBuilder where
    fmap f (ContextPutOutBuilder g) =
        ContextPutOutBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ContextPutOutBuilder where
    pure a = ContextPutOutBuilder (\s -> (s, a))
    (ContextPutOutBuilder f) <*> (ContextPutOutBuilder g) = ContextPutOutBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ContextPutOutBuilder where
    (ContextPutOutBuilder f) >>= g = ContextPutOutBuilder (\s ->
        let (s', a) = f s
            (ContextPutOutBuilder h) = g a
        in h s')

setContextId :: Data.Maybe.Maybe Data.Text.Text -> ContextPutOutBuilder ()
setContextId value =
   ContextPutOutBuilder (\s -> (s { context_idBuilderState = value }, ()))

setOverrideId :: Data.Maybe.Maybe Data.Text.Text -> ContextPutOutBuilder ()
setOverrideId value =
   ContextPutOutBuilder (\s -> (s { override_idBuilderState = value }, ()))

setWeight :: Data.Maybe.Maybe Data.Text.Text -> ContextPutOutBuilder ()
setWeight value =
   ContextPutOutBuilder (\s -> (s { weightBuilderState = value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> ContextPutOutBuilder ()
setDescription value =
   ContextPutOutBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setChangeReason :: Data.Maybe.Maybe Data.Text.Text -> ContextPutOutBuilder ()
setChangeReason value =
   ContextPutOutBuilder (\s -> (s { change_reasonBuilderState = value }, ()))

build :: ContextPutOutBuilder () -> Data.Either.Either Data.Text.Text ContextPutOut
build builder = do
    let (st, _) = runContextPutOutBuilder builder defaultBuilderState
    context_id' <- Data.Either.Right (context_idBuilderState st)
    override_id' <- Data.Either.Right (override_idBuilderState st)
    weight' <- Data.Either.Right (weightBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (ContextPutOut { 
        context_id = context_id',
        override_id = override_id',
        weight = weight',
        description = description',
        change_reason = change_reason'
    })



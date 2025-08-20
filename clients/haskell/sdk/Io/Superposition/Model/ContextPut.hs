module Io.Superposition.Model.ContextPut (
    setContext,
    setOverride,
    setDescription,
    setChangeReason,
    build,
    ContextPutBuilder,
    ContextPut,
    context,
    override,
    description,
    change_reason
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

data ContextPut = ContextPut {
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    override :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ContextPut where
    toJSON a = Data.Aeson.object [
        "context" Data.Aeson..= context a,
        "override" Data.Aeson..= override a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    


instance Data.Aeson.FromJSON ContextPut where
    parseJSON = Data.Aeson.withObject "ContextPut" $ \v -> ContextPut
        Data.Functor.<$> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "override")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data ContextPutBuilderState = ContextPutBuilderState {
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    overrideBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ContextPutBuilderState
defaultBuilderState = ContextPutBuilderState {
    contextBuilderState = Data.Maybe.Nothing,
    overrideBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

newtype ContextPutBuilder a = ContextPutBuilder {
    runContextPutBuilder :: ContextPutBuilderState -> (ContextPutBuilderState, a)
}

instance Data.Functor.Functor ContextPutBuilder where
    fmap f (ContextPutBuilder g) =
        ContextPutBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ContextPutBuilder where
    pure a = ContextPutBuilder (\s -> (s, a))
    (ContextPutBuilder f) <*> (ContextPutBuilder g) = ContextPutBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ContextPutBuilder where
    (ContextPutBuilder f) >>= g = ContextPutBuilder (\s ->
        let (s', a) = f s
            (ContextPutBuilder h) = g a
        in h s')

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> ContextPutBuilder ()
setContext value =
   ContextPutBuilder (\s -> (s { contextBuilderState = Data.Maybe.Just value }, ()))

setOverride :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> ContextPutBuilder ()
setOverride value =
   ContextPutBuilder (\s -> (s { overrideBuilderState = Data.Maybe.Just value }, ()))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> ContextPutBuilder ()
setDescription value =
   ContextPutBuilder (\s -> (s { descriptionBuilderState = value }, ()))

setChangeReason :: Data.Text.Text -> ContextPutBuilder ()
setChangeReason value =
   ContextPutBuilder (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }, ()))

build :: ContextPutBuilder () -> Data.Either.Either Data.Text.Text ContextPut
build builder = do
    let (st, _) = runContextPutBuilder builder defaultBuilderState
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ContextPut.ContextPut.context is a required property.") Data.Either.Right (contextBuilderState st)
    override' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ContextPut.ContextPut.override is a required property.") Data.Either.Right (overrideBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ContextPut.ContextPut.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (ContextPut { 
        context = context',
        override = override',
        description = description',
        change_reason = change_reason'
    })



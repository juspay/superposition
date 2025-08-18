module Io.Superposition.Model.ContextMove (
    setId',
    setContext,
    setDescription,
    setChangeReason,
    build,
    ContextMoveBuilder,
    ContextMove,
    id',
    context,
    description,
    change_reason
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

data ContextMove = ContextMove {
    id' :: Data.Maybe.Maybe Data.Text.Text,
    context :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ContextMove where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "context" Data.Aeson..= context a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    

instance Io.Superposition.Utility.SerializeBody ContextMove

instance Data.Aeson.FromJSON ContextMove where
    parseJSON = Data.Aeson.withObject "ContextMove" $ \v -> ContextMove
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data ContextMoveBuilderState = ContextMoveBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contextBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ContextMoveBuilderState
defaultBuilderState = ContextMoveBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    contextBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

type ContextMoveBuilder = Control.Monad.State.Strict.State ContextMoveBuilderState

setId' :: Data.Maybe.Maybe Data.Text.Text -> ContextMoveBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = value }))

setContext :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> ContextMoveBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> ContextMoveBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = value }))

setChangeReason :: Data.Text.Text -> ContextMoveBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

build :: ContextMoveBuilder () -> Data.Either.Either Data.Text.Text ContextMove
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Either.Right (id'BuilderState st)
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ContextMove.ContextMove.context is a required property.") Data.Either.Right (contextBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ContextMove.ContextMove.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (ContextMove { 
        id' = id'',
        context = context',
        description = description',
        change_reason = change_reason'
    })



module Io.Superposition.Model.ContextPartial (
    setId',
    setCondition,
    setPriority,
    setWeight,
    setOverrideWithKeys,
    build,
    ContextPartialBuilder,
    ContextPartial,
    id',
    condition,
    priority,
    weight,
    override_with_keys
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

data ContextPartial = ContextPartial {
    id' :: Data.Maybe.Maybe Data.Text.Text,
    condition :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    priority :: Data.Maybe.Maybe Data.Int.Int32,
    weight :: Data.Maybe.Maybe Data.Int.Int32,
    override_with_keys :: Data.Maybe.Maybe ([] Data.Text.Text)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ContextPartial where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "condition" Data.Aeson..= condition a,
        "priority" Data.Aeson..= priority a,
        "weight" Data.Aeson..= weight a,
        "override_with_keys" Data.Aeson..= override_with_keys a
        ]
    

instance Io.Superposition.Utility.SerializeBody ContextPartial

instance Data.Aeson.FromJSON ContextPartial where
    parseJSON = Data.Aeson.withObject "ContextPartial" $ \v -> ContextPartial
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "condition")
        Control.Applicative.<*> (v Data.Aeson..: "priority")
        Control.Applicative.<*> (v Data.Aeson..: "weight")
        Control.Applicative.<*> (v Data.Aeson..: "override_with_keys")
    



data ContextPartialBuilderState = ContextPartialBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    conditionBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    priorityBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    weightBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    override_with_keysBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ContextPartialBuilderState
defaultBuilderState = ContextPartialBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    conditionBuilderState = Data.Maybe.Nothing,
    priorityBuilderState = Data.Maybe.Nothing,
    weightBuilderState = Data.Maybe.Nothing,
    override_with_keysBuilderState = Data.Maybe.Nothing
}

type ContextPartialBuilder = Control.Monad.State.Strict.State ContextPartialBuilderState

setId' :: Data.Maybe.Maybe Data.Text.Text -> ContextPartialBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = value }))

setCondition :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> ContextPartialBuilder ()
setCondition value =
   Control.Monad.State.Strict.modify (\s -> (s { conditionBuilderState = value }))

setPriority :: Data.Maybe.Maybe Data.Int.Int32 -> ContextPartialBuilder ()
setPriority value =
   Control.Monad.State.Strict.modify (\s -> (s { priorityBuilderState = value }))

setWeight :: Data.Maybe.Maybe Data.Int.Int32 -> ContextPartialBuilder ()
setWeight value =
   Control.Monad.State.Strict.modify (\s -> (s { weightBuilderState = value }))

setOverrideWithKeys :: Data.Maybe.Maybe ([] Data.Text.Text) -> ContextPartialBuilder ()
setOverrideWithKeys value =
   Control.Monad.State.Strict.modify (\s -> (s { override_with_keysBuilderState = value }))

build :: ContextPartialBuilder () -> Data.Either.Either Data.Text.Text ContextPartial
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Either.Right (id'BuilderState st)
    condition' <- Data.Either.Right (conditionBuilderState st)
    priority' <- Data.Either.Right (priorityBuilderState st)
    weight' <- Data.Either.Right (weightBuilderState st)
    override_with_keys' <- Data.Either.Right (override_with_keysBuilderState st)
    Data.Either.Right (ContextPartial { 
        id' = id'',
        condition = condition',
        priority = priority',
        weight = weight',
        override_with_keys = override_with_keys'
    })



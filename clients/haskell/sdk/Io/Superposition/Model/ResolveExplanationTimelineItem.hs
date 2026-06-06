module Io.Superposition.Model.ResolveExplanationTimelineItem (
    setContextId,
    setCondition,
    setOverrideId,
    setValueBefore,
    setValueAfter,
    build,
    ResolveExplanationTimelineItemBuilder,
    ResolveExplanationTimelineItem,
    context_id,
    condition,
    override_id,
    value_before,
    value_after
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

data ResolveExplanationTimelineItem = ResolveExplanationTimelineItem {
    context_id :: Data.Text.Text,
    condition :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    override_id :: Data.Text.Text,
    value_before :: Data.Aeson.Value,
    value_after :: Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ResolveExplanationTimelineItem where
    toJSON a = Data.Aeson.object [
        "context_id" Data.Aeson..= context_id a,
        "condition" Data.Aeson..= condition a,
        "override_id" Data.Aeson..= override_id a,
        "value_before" Data.Aeson..= value_before a,
        "value_after" Data.Aeson..= value_after a
        ]
    

instance Io.Superposition.Utility.SerializeBody ResolveExplanationTimelineItem

instance Data.Aeson.FromJSON ResolveExplanationTimelineItem where
    parseJSON = Data.Aeson.withObject "ResolveExplanationTimelineItem" $ \v -> ResolveExplanationTimelineItem
        Data.Functor.<$> (v Data.Aeson..: "context_id")
        Control.Applicative.<*> (v Data.Aeson..: "condition")
        Control.Applicative.<*> (v Data.Aeson..: "override_id")
        Control.Applicative.<*> (v Data.Aeson..: "value_before")
        Control.Applicative.<*> (v Data.Aeson..: "value_after")
    



data ResolveExplanationTimelineItemBuilderState = ResolveExplanationTimelineItemBuilderState {
    context_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    conditionBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    override_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    value_beforeBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    value_afterBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ResolveExplanationTimelineItemBuilderState
defaultBuilderState = ResolveExplanationTimelineItemBuilderState {
    context_idBuilderState = Data.Maybe.Nothing,
    conditionBuilderState = Data.Maybe.Nothing,
    override_idBuilderState = Data.Maybe.Nothing,
    value_beforeBuilderState = Data.Maybe.Nothing,
    value_afterBuilderState = Data.Maybe.Nothing
}

type ResolveExplanationTimelineItemBuilder = Control.Monad.State.Strict.State ResolveExplanationTimelineItemBuilderState

setContextId :: Data.Text.Text -> ResolveExplanationTimelineItemBuilder ()
setContextId value =
   Control.Monad.State.Strict.modify (\s -> (s { context_idBuilderState = Data.Maybe.Just value }))

setCondition :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> ResolveExplanationTimelineItemBuilder ()
setCondition value =
   Control.Monad.State.Strict.modify (\s -> (s { conditionBuilderState = Data.Maybe.Just value }))

setOverrideId :: Data.Text.Text -> ResolveExplanationTimelineItemBuilder ()
setOverrideId value =
   Control.Monad.State.Strict.modify (\s -> (s { override_idBuilderState = Data.Maybe.Just value }))

setValueBefore :: Data.Aeson.Value -> ResolveExplanationTimelineItemBuilder ()
setValueBefore value =
   Control.Monad.State.Strict.modify (\s -> (s { value_beforeBuilderState = Data.Maybe.Just value }))

setValueAfter :: Data.Aeson.Value -> ResolveExplanationTimelineItemBuilder ()
setValueAfter value =
   Control.Monad.State.Strict.modify (\s -> (s { value_afterBuilderState = Data.Maybe.Just value }))

build :: ResolveExplanationTimelineItemBuilder () -> Data.Either.Either Data.Text.Text ResolveExplanationTimelineItem
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    context_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResolveExplanationTimelineItem.ResolveExplanationTimelineItem.context_id is a required property.") Data.Either.Right (context_idBuilderState st)
    condition' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResolveExplanationTimelineItem.ResolveExplanationTimelineItem.condition is a required property.") Data.Either.Right (conditionBuilderState st)
    override_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResolveExplanationTimelineItem.ResolveExplanationTimelineItem.override_id is a required property.") Data.Either.Right (override_idBuilderState st)
    value_before' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResolveExplanationTimelineItem.ResolveExplanationTimelineItem.value_before is a required property.") Data.Either.Right (value_beforeBuilderState st)
    value_after' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResolveExplanationTimelineItem.ResolveExplanationTimelineItem.value_after is a required property.") Data.Either.Right (value_afterBuilderState st)
    Data.Either.Right (ResolveExplanationTimelineItem { 
        context_id = context_id',
        condition = condition',
        override_id = override_id',
        value_before = value_before',
        value_after = value_after'
    })



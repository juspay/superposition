module Io.Superposition.Model.ResolveExplanation (
    setKey,
    setTimeline,
    build,
    ResolveExplanationBuilder,
    ResolveExplanation,
    key,
    timeline
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ResolveExplanationTimelineItem
import qualified Io.Superposition.Utility

data ResolveExplanation = ResolveExplanation {
    key :: Data.Text.Text,
    timeline :: [] Io.Superposition.Model.ResolveExplanationTimelineItem.ResolveExplanationTimelineItem
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ResolveExplanation where
    toJSON a = Data.Aeson.object [
        "key" Data.Aeson..= key a,
        "timeline" Data.Aeson..= timeline a
        ]
    

instance Io.Superposition.Utility.SerializeBody ResolveExplanation

instance Data.Aeson.FromJSON ResolveExplanation where
    parseJSON = Data.Aeson.withObject "ResolveExplanation" $ \v -> ResolveExplanation
        Data.Functor.<$> (v Data.Aeson..: "key")
        Control.Applicative.<*> (v Data.Aeson..: "timeline")
    



data ResolveExplanationBuilderState = ResolveExplanationBuilderState {
    keyBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    timelineBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ResolveExplanationTimelineItem.ResolveExplanationTimelineItem)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ResolveExplanationBuilderState
defaultBuilderState = ResolveExplanationBuilderState {
    keyBuilderState = Data.Maybe.Nothing,
    timelineBuilderState = Data.Maybe.Nothing
}

type ResolveExplanationBuilder = Control.Monad.State.Strict.State ResolveExplanationBuilderState

setKey :: Data.Text.Text -> ResolveExplanationBuilder ()
setKey value =
   Control.Monad.State.Strict.modify (\s -> (s { keyBuilderState = Data.Maybe.Just value }))

setTimeline :: [] Io.Superposition.Model.ResolveExplanationTimelineItem.ResolveExplanationTimelineItem -> ResolveExplanationBuilder ()
setTimeline value =
   Control.Monad.State.Strict.modify (\s -> (s { timelineBuilderState = Data.Maybe.Just value }))

build :: ResolveExplanationBuilder () -> Data.Either.Either Data.Text.Text ResolveExplanation
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    key' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResolveExplanation.ResolveExplanation.key is a required property.") Data.Either.Right (keyBuilderState st)
    timeline' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ResolveExplanation.ResolveExplanation.timeline is a required property.") Data.Either.Right (timelineBuilderState st)
    Data.Either.Right (ResolveExplanation { 
        key = key',
        timeline = timeline'
    })



module Io.Superposition.Model.WeightRecomputeResponse (
    setId',
    setCondition,
    setOldWeight,
    setNewWeight,
    build,
    WeightRecomputeResponseBuilder,
    WeightRecomputeResponse,
    id',
    condition,
    old_weight,
    new_weight
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

data WeightRecomputeResponse = WeightRecomputeResponse {
    id' :: Data.Maybe.Maybe Data.Text.Text,
    condition :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    old_weight :: Data.Maybe.Maybe Data.Text.Text,
    new_weight :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WeightRecomputeResponse where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "condition" Data.Aeson..= condition a,
        "old_weight" Data.Aeson..= old_weight a,
        "new_weight" Data.Aeson..= new_weight a
        ]
    

instance Io.Superposition.Utility.SerializeBody WeightRecomputeResponse

instance Data.Aeson.FromJSON WeightRecomputeResponse where
    parseJSON = Data.Aeson.withObject "WeightRecomputeResponse" $ \v -> WeightRecomputeResponse
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "condition")
        Control.Applicative.<*> (v Data.Aeson..: "old_weight")
        Control.Applicative.<*> (v Data.Aeson..: "new_weight")
    



data WeightRecomputeResponseBuilderState = WeightRecomputeResponseBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    conditionBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    old_weightBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    new_weightBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WeightRecomputeResponseBuilderState
defaultBuilderState = WeightRecomputeResponseBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    conditionBuilderState = Data.Maybe.Nothing,
    old_weightBuilderState = Data.Maybe.Nothing,
    new_weightBuilderState = Data.Maybe.Nothing
}

type WeightRecomputeResponseBuilder = Control.Monad.State.Strict.State WeightRecomputeResponseBuilderState

setId' :: Data.Maybe.Maybe Data.Text.Text -> WeightRecomputeResponseBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = value }))

setCondition :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> WeightRecomputeResponseBuilder ()
setCondition value =
   Control.Monad.State.Strict.modify (\s -> (s { conditionBuilderState = value }))

setOldWeight :: Data.Maybe.Maybe Data.Text.Text -> WeightRecomputeResponseBuilder ()
setOldWeight value =
   Control.Monad.State.Strict.modify (\s -> (s { old_weightBuilderState = value }))

setNewWeight :: Data.Maybe.Maybe Data.Text.Text -> WeightRecomputeResponseBuilder ()
setNewWeight value =
   Control.Monad.State.Strict.modify (\s -> (s { new_weightBuilderState = value }))

build :: WeightRecomputeResponseBuilder () -> Data.Either.Either Data.Text.Text WeightRecomputeResponse
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Either.Right (id'BuilderState st)
    condition' <- Data.Either.Right (conditionBuilderState st)
    old_weight' <- Data.Either.Right (old_weightBuilderState st)
    new_weight' <- Data.Either.Right (new_weightBuilderState st)
    Data.Either.Right (WeightRecomputeResponse { 
        id' = id'',
        condition = condition',
        old_weight = old_weight',
        new_weight = new_weight'
    })



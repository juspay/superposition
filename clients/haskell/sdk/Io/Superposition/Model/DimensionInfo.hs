module Io.Superposition.Model.DimensionInfo (
    setSchema,
    setPosition,
    setDimensionType,
    setDependencyGraph,
    build,
    DimensionInfoBuilder,
    DimensionInfo,
    schema,
    position,
    dimension_type,
    dependency_graph
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
import qualified Io.Superposition.Model.DimensionType
import qualified Io.Superposition.Utility

data DimensionInfo = DimensionInfo {
    schema :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    position :: Data.Maybe.Maybe Data.Int.Int32,
    dimension_type :: Data.Maybe.Maybe Io.Superposition.Model.DimensionType.DimensionType,
    dependency_graph :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text ([] Data.Text.Text))
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DimensionInfo where
    toJSON a = Data.Aeson.object [
        "schema" Data.Aeson..= schema a,
        "position" Data.Aeson..= position a,
        "dimension_type" Data.Aeson..= dimension_type a,
        "dependency_graph" Data.Aeson..= dependency_graph a
        ]
    

instance Io.Superposition.Utility.SerializeBody DimensionInfo

instance Data.Aeson.FromJSON DimensionInfo where
    parseJSON = Data.Aeson.withObject "DimensionInfo" $ \v -> DimensionInfo
        Data.Functor.<$> (v Data.Aeson..: "schema")
        Control.Applicative.<*> (v Data.Aeson..: "position")
        Control.Applicative.<*> (v Data.Aeson..: "dimension_type")
        Control.Applicative.<*> (v Data.Aeson..: "dependency_graph")
    



data DimensionInfoBuilderState = DimensionInfoBuilderState {
    schemaBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    positionBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    dimension_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.DimensionType.DimensionType,
    dependency_graphBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text ([] Data.Text.Text))
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DimensionInfoBuilderState
defaultBuilderState = DimensionInfoBuilderState {
    schemaBuilderState = Data.Maybe.Nothing,
    positionBuilderState = Data.Maybe.Nothing,
    dimension_typeBuilderState = Data.Maybe.Nothing,
    dependency_graphBuilderState = Data.Maybe.Nothing
}

type DimensionInfoBuilder = Control.Monad.State.Strict.State DimensionInfoBuilderState

setSchema :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) -> DimensionInfoBuilder ()
setSchema value =
   Control.Monad.State.Strict.modify (\s -> (s { schemaBuilderState = value }))

setPosition :: Data.Maybe.Maybe Data.Int.Int32 -> DimensionInfoBuilder ()
setPosition value =
   Control.Monad.State.Strict.modify (\s -> (s { positionBuilderState = value }))

setDimensionType :: Data.Maybe.Maybe Io.Superposition.Model.DimensionType.DimensionType -> DimensionInfoBuilder ()
setDimensionType value =
   Control.Monad.State.Strict.modify (\s -> (s { dimension_typeBuilderState = value }))

setDependencyGraph :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text ([] Data.Text.Text)) -> DimensionInfoBuilder ()
setDependencyGraph value =
   Control.Monad.State.Strict.modify (\s -> (s { dependency_graphBuilderState = value }))

build :: DimensionInfoBuilder () -> Data.Either.Either Data.Text.Text DimensionInfo
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    schema' <- Data.Either.Right (schemaBuilderState st)
    position' <- Data.Either.Right (positionBuilderState st)
    dimension_type' <- Data.Either.Right (dimension_typeBuilderState st)
    dependency_graph' <- Data.Either.Right (dependency_graphBuilderState st)
    Data.Either.Right (DimensionInfo { 
        schema = schema',
        position = position',
        dimension_type = dimension_type',
        dependency_graph = dependency_graph'
    })



module Io.Superposition.Model.Unit (
    build,
    UnitBuilder,
    Unit
) where
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

data Unit = Unit {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON Unit where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody Unit

instance Data.Aeson.FromJSON Unit where
    parseJSON = Data.Aeson.withObject "Unit" $ \_ -> pure $ Unit



data UnitBuilderState = UnitBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UnitBuilderState
defaultBuilderState = UnitBuilderState {
}

type UnitBuilder = Control.Monad.State.Strict.State UnitBuilderState


build :: UnitBuilder () -> Data.Either.Either Data.Text.Text Unit
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (Unit { 
    })



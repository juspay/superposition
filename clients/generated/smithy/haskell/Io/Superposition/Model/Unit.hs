module Io.Superposition.Model.Unit (
    build,
    UnitBuilder,
    Unit
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data Unit = Unit {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON Unit where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON Unit where
    parseJSON = Data.Aeson.withObject "Unit" $ \_ -> pure $ Unit



data UnitBuilderState = UnitBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UnitBuilderState
defaultBuilderState = UnitBuilderState {
}

newtype UnitBuilder a = UnitBuilder {
    runUnitBuilder :: UnitBuilderState -> (UnitBuilderState, a)
}

instance Data.Functor.Functor UnitBuilder where
    fmap f (UnitBuilder g) =
        UnitBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative UnitBuilder where
    pure a = UnitBuilder (\s -> (s, a))
    (UnitBuilder f) <*> (UnitBuilder g) = UnitBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad UnitBuilder where
    (UnitBuilder f) >>= g = UnitBuilder (\s ->
        let (s', a) = f s
            (UnitBuilder h) = g a
        in h s')


build :: UnitBuilder () -> Data.Either.Either Data.Text.Text Unit
build builder = do
    let (st, _) = runUnitBuilder builder defaultBuilderState
    Data.Either.Right (Unit { 
    })



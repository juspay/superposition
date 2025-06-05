module Io.Superposition.Model.ApplicableVariantsOutput (
    setData',
    build,
    ApplicableVariantsOutputBuilder,
    ApplicableVariantsOutput,
    data'
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
import qualified Io.Superposition.Model.Variant

data ApplicableVariantsOutput = ApplicableVariantsOutput {
    data' :: [] Io.Superposition.Model.Variant.Variant
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ApplicableVariantsOutput where
    toJSON a = Data.Aeson.object [
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON ApplicableVariantsOutput where
    parseJSON = Data.Aeson.withObject "ApplicableVariantsOutput" $ \v -> ApplicableVariantsOutput
        Data.Functor.<$> (v Data.Aeson..: "data")
    



data ApplicableVariantsOutputBuilderState = ApplicableVariantsOutputBuilderState {
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.Variant.Variant)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ApplicableVariantsOutputBuilderState
defaultBuilderState = ApplicableVariantsOutputBuilderState {
    data'BuilderState = Data.Maybe.Nothing
}

newtype ApplicableVariantsOutputBuilder a = ApplicableVariantsOutputBuilder {
    runApplicableVariantsOutputBuilder :: ApplicableVariantsOutputBuilderState -> (ApplicableVariantsOutputBuilderState, a)
}

instance Data.Functor.Functor ApplicableVariantsOutputBuilder where
    fmap f (ApplicableVariantsOutputBuilder g) =
        ApplicableVariantsOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ApplicableVariantsOutputBuilder where
    pure a = ApplicableVariantsOutputBuilder (\s -> (s, a))
    (ApplicableVariantsOutputBuilder f) <*> (ApplicableVariantsOutputBuilder g) = ApplicableVariantsOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ApplicableVariantsOutputBuilder where
    (ApplicableVariantsOutputBuilder f) >>= g = ApplicableVariantsOutputBuilder (\s ->
        let (s', a) = f s
            (ApplicableVariantsOutputBuilder h) = g a
        in h s')

setData' :: [] Io.Superposition.Model.Variant.Variant -> ApplicableVariantsOutputBuilder ()
setData' value =
   ApplicableVariantsOutputBuilder (\s -> (s { data'BuilderState = Data.Maybe.Just value }, ()))

build :: ApplicableVariantsOutputBuilder () -> Data.Either.Either Data.Text.Text ApplicableVariantsOutput
build builder = do
    let (st, _) = runApplicableVariantsOutputBuilder builder defaultBuilderState
    data'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ApplicableVariantsOutput.ApplicableVariantsOutput.data' is a required property.") Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ApplicableVariantsOutput { 
        data' = data''
    })



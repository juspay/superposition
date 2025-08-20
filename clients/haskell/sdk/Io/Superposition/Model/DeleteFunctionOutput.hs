module Io.Superposition.Model.DeleteFunctionOutput (
    build,
    DeleteFunctionOutputBuilder,
    DeleteFunctionOutput
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

data DeleteFunctionOutput = DeleteFunctionOutput {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON DeleteFunctionOutput where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON DeleteFunctionOutput where
    parseJSON = Data.Aeson.withObject "DeleteFunctionOutput" $ \_ -> pure $ DeleteFunctionOutput



data DeleteFunctionOutputBuilderState = DeleteFunctionOutputBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: DeleteFunctionOutputBuilderState
defaultBuilderState = DeleteFunctionOutputBuilderState {
}

newtype DeleteFunctionOutputBuilder a = DeleteFunctionOutputBuilder {
    runDeleteFunctionOutputBuilder :: DeleteFunctionOutputBuilderState -> (DeleteFunctionOutputBuilderState, a)
}

instance Data.Functor.Functor DeleteFunctionOutputBuilder where
    fmap f (DeleteFunctionOutputBuilder g) =
        DeleteFunctionOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative DeleteFunctionOutputBuilder where
    pure a = DeleteFunctionOutputBuilder (\s -> (s, a))
    (DeleteFunctionOutputBuilder f) <*> (DeleteFunctionOutputBuilder g) = DeleteFunctionOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad DeleteFunctionOutputBuilder where
    (DeleteFunctionOutputBuilder f) >>= g = DeleteFunctionOutputBuilder (\s ->
        let (s', a) = f s
            (DeleteFunctionOutputBuilder h) = g a
        in h s')


build :: DeleteFunctionOutputBuilder () -> Data.Either.Either Data.Text.Text DeleteFunctionOutput
build builder = do
    let (st, _) = runDeleteFunctionOutputBuilder builder defaultBuilderState
    Data.Either.Right (DeleteFunctionOutput { 
    })



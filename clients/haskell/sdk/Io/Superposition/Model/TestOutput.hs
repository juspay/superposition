module Io.Superposition.Model.TestOutput (
    setFnOutput,
    setStdout,
    setFunctionType,
    build,
    TestOutputBuilder,
    TestOutput,
    fn_output,
    stdout,
    function_type
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
import qualified Io.Superposition.Model.FunctionTypes

data TestOutput = TestOutput {
    fn_output :: Data.Aeson.Value,
    stdout :: Data.Text.Text,
    function_type :: Io.Superposition.Model.FunctionTypes.FunctionTypes
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestOutput where
    toJSON a = Data.Aeson.object [
        "fn_output" Data.Aeson..= fn_output a,
        "stdout" Data.Aeson..= stdout a,
        "function_type" Data.Aeson..= function_type a
        ]
    


instance Data.Aeson.FromJSON TestOutput where
    parseJSON = Data.Aeson.withObject "TestOutput" $ \v -> TestOutput
        Data.Functor.<$> (v Data.Aeson..: "fn_output")
        Control.Applicative.<*> (v Data.Aeson..: "stdout")
        Control.Applicative.<*> (v Data.Aeson..: "function_type")
    



data TestOutputBuilderState = TestOutputBuilderState {
    fn_outputBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    stdoutBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.FunctionTypes.FunctionTypes
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestOutputBuilderState
defaultBuilderState = TestOutputBuilderState {
    fn_outputBuilderState = Data.Maybe.Nothing,
    stdoutBuilderState = Data.Maybe.Nothing,
    function_typeBuilderState = Data.Maybe.Nothing
}

newtype TestOutputBuilder a = TestOutputBuilder {
    runTestOutputBuilder :: TestOutputBuilderState -> (TestOutputBuilderState, a)
}

instance Data.Functor.Functor TestOutputBuilder where
    fmap f (TestOutputBuilder g) =
        TestOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestOutputBuilder where
    pure a = TestOutputBuilder (\s -> (s, a))
    (TestOutputBuilder f) <*> (TestOutputBuilder g) = TestOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestOutputBuilder where
    (TestOutputBuilder f) >>= g = TestOutputBuilder (\s ->
        let (s', a) = f s
            (TestOutputBuilder h) = g a
        in h s')

setFnOutput :: Data.Aeson.Value -> TestOutputBuilder ()
setFnOutput value =
   TestOutputBuilder (\s -> (s { fn_outputBuilderState = Data.Maybe.Just value }, ()))

setStdout :: Data.Text.Text -> TestOutputBuilder ()
setStdout value =
   TestOutputBuilder (\s -> (s { stdoutBuilderState = Data.Maybe.Just value }, ()))

setFunctionType :: Io.Superposition.Model.FunctionTypes.FunctionTypes -> TestOutputBuilder ()
setFunctionType value =
   TestOutputBuilder (\s -> (s { function_typeBuilderState = Data.Maybe.Just value }, ()))

build :: TestOutputBuilder () -> Data.Either.Either Data.Text.Text TestOutput
build builder = do
    let (st, _) = runTestOutputBuilder builder defaultBuilderState
    fn_output' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestOutput.TestOutput.fn_output is a required property.") Data.Either.Right (fn_outputBuilderState st)
    stdout' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestOutput.TestOutput.stdout is a required property.") Data.Either.Right (stdoutBuilderState st)
    function_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestOutput.TestOutput.function_type is a required property.") Data.Either.Right (function_typeBuilderState st)
    Data.Either.Right (TestOutput { 
        fn_output = fn_output',
        stdout = stdout',
        function_type = function_type'
    })



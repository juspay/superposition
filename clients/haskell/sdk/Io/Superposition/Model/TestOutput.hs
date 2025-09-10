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
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.FunctionTypes
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

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
    

instance Io.Superposition.Utility.SerializeBody TestOutput

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

type TestOutputBuilder = Control.Monad.State.Strict.State TestOutputBuilderState

setFnOutput :: Data.Aeson.Value -> TestOutputBuilder ()
setFnOutput value =
   Control.Monad.State.Strict.modify (\s -> (s { fn_outputBuilderState = Data.Maybe.Just value }))

setStdout :: Data.Text.Text -> TestOutputBuilder ()
setStdout value =
   Control.Monad.State.Strict.modify (\s -> (s { stdoutBuilderState = Data.Maybe.Just value }))

setFunctionType :: Io.Superposition.Model.FunctionTypes.FunctionTypes -> TestOutputBuilder ()
setFunctionType value =
   Control.Monad.State.Strict.modify (\s -> (s { function_typeBuilderState = Data.Maybe.Just value }))

build :: TestOutputBuilder () -> Data.Either.Either Data.Text.Text TestOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    fn_output' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestOutput.TestOutput.fn_output is a required property.") Data.Either.Right (fn_outputBuilderState st)
    stdout' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestOutput.TestOutput.stdout is a required property.") Data.Either.Right (stdoutBuilderState st)
    function_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestOutput.TestOutput.function_type is a required property.") Data.Either.Right (function_typeBuilderState st)
    Data.Either.Right (TestOutput { 
        fn_output = fn_output',
        stdout = stdout',
        function_type = function_type'
    })


instance Io.Superposition.Utility.FromResponseParser TestOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "stdout"
        var1 <- Io.Superposition.Utility.deSerField "function_type"
        var2 <- Io.Superposition.Utility.deSerField "fn_output"
        pure $ TestOutput {
            fn_output = var2,
            stdout = var0,
            function_type = var1
        }


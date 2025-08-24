module Io.Superposition.Model.TestInput (
    setWorkspaceId,
    setOrgId,
    setFunctionName,
    setStage,
    setRequest,
    build,
    TestInputBuilder,
    TestInput,
    workspace_id,
    org_id,
    function_name,
    stage,
    request
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
import qualified Io.Superposition.Model.FunctionExecutionRequest
import qualified Io.Superposition.Model.Stage

data TestInput = TestInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    function_name :: Data.Text.Text,
    stage :: Io.Superposition.Model.Stage.Stage,
    request :: Io.Superposition.Model.FunctionExecutionRequest.FunctionExecutionRequest
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "function_name" Data.Aeson..= function_name a,
        "stage" Data.Aeson..= stage a,
        "request" Data.Aeson..= request a
        ]
    


instance Data.Aeson.FromJSON TestInput where
    parseJSON = Data.Aeson.withObject "TestInput" $ \v -> TestInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "function_name")
        Control.Applicative.<*> (v Data.Aeson..: "stage")
        Control.Applicative.<*> (v Data.Aeson..: "request")
    



data TestInputBuilderState = TestInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    function_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    stageBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.Stage.Stage,
    requestBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.FunctionExecutionRequest.FunctionExecutionRequest
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestInputBuilderState
defaultBuilderState = TestInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    function_nameBuilderState = Data.Maybe.Nothing,
    stageBuilderState = Data.Maybe.Nothing,
    requestBuilderState = Data.Maybe.Nothing
}

newtype TestInputBuilder a = TestInputBuilder {
    runTestInputBuilder :: TestInputBuilderState -> (TestInputBuilderState, a)
}

instance Data.Functor.Functor TestInputBuilder where
    fmap f (TestInputBuilder g) =
        TestInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestInputBuilder where
    pure a = TestInputBuilder (\s -> (s, a))
    (TestInputBuilder f) <*> (TestInputBuilder g) = TestInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestInputBuilder where
    (TestInputBuilder f) >>= g = TestInputBuilder (\s ->
        let (s', a) = f s
            (TestInputBuilder h) = g a
        in h s')

setWorkspaceId :: Data.Text.Text -> TestInputBuilder ()
setWorkspaceId value =
   TestInputBuilder (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }, ()))

setOrgId :: Data.Text.Text -> TestInputBuilder ()
setOrgId value =
   TestInputBuilder (\s -> (s { org_idBuilderState = Data.Maybe.Just value }, ()))

setFunctionName :: Data.Text.Text -> TestInputBuilder ()
setFunctionName value =
   TestInputBuilder (\s -> (s { function_nameBuilderState = Data.Maybe.Just value }, ()))

setStage :: Io.Superposition.Model.Stage.Stage -> TestInputBuilder ()
setStage value =
   TestInputBuilder (\s -> (s { stageBuilderState = Data.Maybe.Just value }, ()))

setRequest :: Io.Superposition.Model.FunctionExecutionRequest.FunctionExecutionRequest -> TestInputBuilder ()
setRequest value =
   TestInputBuilder (\s -> (s { requestBuilderState = Data.Maybe.Just value }, ()))

build :: TestInputBuilder () -> Data.Either.Either Data.Text.Text TestInput
build builder = do
    let (st, _) = runTestInputBuilder builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestInput.TestInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestInput.TestInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    function_name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestInput.TestInput.function_name is a required property.") Data.Either.Right (function_nameBuilderState st)
    stage' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestInput.TestInput.stage is a required property.") Data.Either.Right (stageBuilderState st)
    request' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.TestInput.TestInput.request is a required property.") Data.Either.Right (requestBuilderState st)
    Data.Either.Right (TestInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        function_name = function_name',
        stage = stage',
        request = request'
    })



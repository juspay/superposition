module Io.Superposition.Model.GetExperimentConfigOutput (
    setLastModified,
    setExperiments,
    setExperimentGroups,
    build,
    GetExperimentConfigOutputBuilder,
    GetExperimentConfigOutput,
    last_modified,
    experiments,
    experiment_groups
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ExperimentGroupResponse
import qualified Io.Superposition.Model.ExperimentResponse
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data GetExperimentConfigOutput = GetExperimentConfigOutput {
    last_modified :: Data.Time.UTCTime,
    experiments :: [] Io.Superposition.Model.ExperimentResponse.ExperimentResponse,
    experiment_groups :: [] Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetExperimentConfigOutput where
    toJSON a = Data.Aeson.object [
        "last_modified" Data.Aeson..= last_modified a,
        "experiments" Data.Aeson..= experiments a,
        "experiment_groups" Data.Aeson..= experiment_groups a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetExperimentConfigOutput

instance Data.Aeson.FromJSON GetExperimentConfigOutput where
    parseJSON = Data.Aeson.withObject "GetExperimentConfigOutput" $ \v -> GetExperimentConfigOutput
        Data.Functor.<$> (v Data.Aeson..: "last_modified")
        Control.Applicative.<*> (v Data.Aeson..: "experiments")
        Control.Applicative.<*> (v Data.Aeson..: "experiment_groups")
    



data GetExperimentConfigOutputBuilderState = GetExperimentConfigOutputBuilderState {
    last_modifiedBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    experimentsBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ExperimentResponse.ExperimentResponse),
    experiment_groupsBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetExperimentConfigOutputBuilderState
defaultBuilderState = GetExperimentConfigOutputBuilderState {
    last_modifiedBuilderState = Data.Maybe.Nothing,
    experimentsBuilderState = Data.Maybe.Nothing,
    experiment_groupsBuilderState = Data.Maybe.Nothing
}

type GetExperimentConfigOutputBuilder = Control.Monad.State.Strict.State GetExperimentConfigOutputBuilderState

setLastModified :: Data.Time.UTCTime -> GetExperimentConfigOutputBuilder ()
setLastModified value =
   Control.Monad.State.Strict.modify (\s -> (s { last_modifiedBuilderState = Data.Maybe.Just value }))

setExperiments :: [] Io.Superposition.Model.ExperimentResponse.ExperimentResponse -> GetExperimentConfigOutputBuilder ()
setExperiments value =
   Control.Monad.State.Strict.modify (\s -> (s { experimentsBuilderState = Data.Maybe.Just value }))

setExperimentGroups :: [] Io.Superposition.Model.ExperimentGroupResponse.ExperimentGroupResponse -> GetExperimentConfigOutputBuilder ()
setExperimentGroups value =
   Control.Monad.State.Strict.modify (\s -> (s { experiment_groupsBuilderState = Data.Maybe.Just value }))

build :: GetExperimentConfigOutputBuilder () -> Data.Either.Either Data.Text.Text GetExperimentConfigOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    last_modified' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetExperimentConfigOutput.GetExperimentConfigOutput.last_modified is a required property.") Data.Either.Right (last_modifiedBuilderState st)
    experiments' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetExperimentConfigOutput.GetExperimentConfigOutput.experiments is a required property.") Data.Either.Right (experimentsBuilderState st)
    experiment_groups' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetExperimentConfigOutput.GetExperimentConfigOutput.experiment_groups is a required property.") Data.Either.Right (experiment_groupsBuilderState st)
    Data.Either.Right (GetExperimentConfigOutput { 
        last_modified = last_modified',
        experiments = experiments',
        experiment_groups = experiment_groups'
    })


instance Io.Superposition.Utility.FromResponseParser GetExperimentConfigOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        var0 <- Io.Superposition.Utility.deSerHeader "last-modified"
        var1 <- Io.Superposition.Utility.deSerField "experiment_groups"
        var2 <- Io.Superposition.Utility.deSerField "experiments"
        pure $ GetExperimentConfigOutput {
            last_modified = var0,
            experiments = var2,
            experiment_groups = var1
        }


module Io.Superposition.Model.WeightRecomputeOutput (
    setId',
    setKronosJobId,
    setStatus,
    build,
    WeightRecomputeOutputBuilder,
    WeightRecomputeOutput,
    id',
    kronos_job_id,
    status
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
import qualified Io.Superposition.Model.BackgroundJobStatus
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data WeightRecomputeOutput = WeightRecomputeOutput {
    id' :: Data.Text.Text,
    kronos_job_id :: Data.Text.Text,
    status :: Io.Superposition.Model.BackgroundJobStatus.BackgroundJobStatus
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WeightRecomputeOutput where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "kronos_job_id" Data.Aeson..= kronos_job_id a,
        "status" Data.Aeson..= status a
        ]
    

instance Io.Superposition.Utility.SerializeBody WeightRecomputeOutput

instance Data.Aeson.FromJSON WeightRecomputeOutput where
    parseJSON = Data.Aeson.withObject "WeightRecomputeOutput" $ \v -> WeightRecomputeOutput
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "kronos_job_id")
        Control.Applicative.<*> (v Data.Aeson..: "status")
    



data WeightRecomputeOutputBuilderState = WeightRecomputeOutputBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    kronos_job_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.BackgroundJobStatus.BackgroundJobStatus
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WeightRecomputeOutputBuilderState
defaultBuilderState = WeightRecomputeOutputBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    kronos_job_idBuilderState = Data.Maybe.Nothing,
    statusBuilderState = Data.Maybe.Nothing
}

type WeightRecomputeOutputBuilder = Control.Monad.State.Strict.State WeightRecomputeOutputBuilderState

setId' :: Data.Text.Text -> WeightRecomputeOutputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setKronosJobId :: Data.Text.Text -> WeightRecomputeOutputBuilder ()
setKronosJobId value =
   Control.Monad.State.Strict.modify (\s -> (s { kronos_job_idBuilderState = Data.Maybe.Just value }))

setStatus :: Io.Superposition.Model.BackgroundJobStatus.BackgroundJobStatus -> WeightRecomputeOutputBuilder ()
setStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { statusBuilderState = Data.Maybe.Just value }))

build :: WeightRecomputeOutputBuilder () -> Data.Either.Either Data.Text.Text WeightRecomputeOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WeightRecomputeOutput.WeightRecomputeOutput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    kronos_job_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WeightRecomputeOutput.WeightRecomputeOutput.kronos_job_id is a required property.") Data.Either.Right (kronos_job_idBuilderState st)
    status' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.WeightRecomputeOutput.WeightRecomputeOutput.status is a required property.") Data.Either.Right (statusBuilderState st)
    Data.Either.Right (WeightRecomputeOutput { 
        id' = id'',
        kronos_job_id = kronos_job_id',
        status = status'
    })


instance Io.Superposition.Utility.FromResponseParser WeightRecomputeOutput where
    expectedStatus = (Network.HTTP.Types.mkStatus 200 "")
    responseParser = do
        
        var0 <- Io.Superposition.Utility.deSerField "kronos_job_id"
        var1 <- Io.Superposition.Utility.deSerField "id"
        var2 <- Io.Superposition.Utility.deSerField "status"
        pure $ WeightRecomputeOutput {
            id' = var1,
            kronos_job_id = var0,
            status = var2
        }


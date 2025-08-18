module Io.Superposition.Model.RampExperimentInput (
    setWorkspaceId,
    setOrgId,
    setId',
    setChangeReason,
    setTrafficPercentage,
    build,
    RampExperimentInputBuilder,
    RampExperimentInput,
    workspace_id,
    org_id,
    id',
    change_reason,
    traffic_percentage
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data RampExperimentInput = RampExperimentInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    id' :: Data.Text.Text,
    change_reason :: Data.Text.Text,
    traffic_percentage :: Data.Int.Int32
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON RampExperimentInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "id" Data.Aeson..= id' a,
        "change_reason" Data.Aeson..= change_reason a,
        "traffic_percentage" Data.Aeson..= traffic_percentage a
        ]
    

instance Io.Superposition.Utility.SerializeBody RampExperimentInput

instance Data.Aeson.FromJSON RampExperimentInput where
    parseJSON = Data.Aeson.withObject "RampExperimentInput" $ \v -> RampExperimentInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
        Control.Applicative.<*> (v Data.Aeson..: "traffic_percentage")
    



data RampExperimentInputBuilderState = RampExperimentInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    traffic_percentageBuilderState :: Data.Maybe.Maybe Data.Int.Int32
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: RampExperimentInputBuilderState
defaultBuilderState = RampExperimentInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing,
    traffic_percentageBuilderState = Data.Maybe.Nothing
}

type RampExperimentInputBuilder = Control.Monad.State.Strict.State RampExperimentInputBuilderState

setWorkspaceId :: Data.Text.Text -> RampExperimentInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> RampExperimentInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setId' :: Data.Text.Text -> RampExperimentInputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setChangeReason :: Data.Text.Text -> RampExperimentInputBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

setTrafficPercentage :: Data.Int.Int32 -> RampExperimentInputBuilder ()
setTrafficPercentage value =
   Control.Monad.State.Strict.modify (\s -> (s { traffic_percentageBuilderState = Data.Maybe.Just value }))

build :: RampExperimentInputBuilder () -> Data.Either.Either Data.Text.Text RampExperimentInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentInput.RampExperimentInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentInput.RampExperimentInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentInput.RampExperimentInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentInput.RampExperimentInput.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    traffic_percentage' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.RampExperimentInput.RampExperimentInput.traffic_percentage is a required property.") Data.Either.Right (traffic_percentageBuilderState st)
    Data.Either.Right (RampExperimentInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        id' = id'',
        change_reason = change_reason',
        traffic_percentage = traffic_percentage'
    })


instance Io.Superposition.Utility.IntoRequestBuilder RampExperimentInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPatch
        Io.Superposition.Utility.setPath [
            "experiments",
            Io.Superposition.Utility.serializeElement (id' self),
            "ramp"
            ]
        
        Io.Superposition.Utility.serHeader "x-tenant" (workspace_id self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        Io.Superposition.Utility.serField "change_reason" (change_reason self)
        Io.Superposition.Utility.serField "traffic_percentage" (traffic_percentage self)


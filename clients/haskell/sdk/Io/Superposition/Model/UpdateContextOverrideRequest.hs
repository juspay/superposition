module Io.Superposition.Model.UpdateContextOverrideRequest (
    setContext,
    setOverride,
    setDescription,
    setChangeReason,
    build,
    UpdateContextOverrideRequestBuilder,
    UpdateContextOverrideRequest,
    context,
    override,
    description,
    change_reason
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ContextIdentifier
import qualified Io.Superposition.Utility

data UpdateContextOverrideRequest = UpdateContextOverrideRequest {
    context :: Io.Superposition.Model.ContextIdentifier.ContextIdentifier,
    override :: Data.Map.Map Data.Text.Text Data.Aeson.Value,
    description :: Data.Maybe.Maybe Data.Text.Text,
    change_reason :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateContextOverrideRequest where
    toJSON a = Data.Aeson.object [
        "context" Data.Aeson..= context a,
        "override" Data.Aeson..= override a,
        "description" Data.Aeson..= description a,
        "change_reason" Data.Aeson..= change_reason a
        ]
    

instance Io.Superposition.Utility.SerializeBody UpdateContextOverrideRequest

instance Data.Aeson.FromJSON UpdateContextOverrideRequest where
    parseJSON = Data.Aeson.withObject "UpdateContextOverrideRequest" $ \v -> UpdateContextOverrideRequest
        Data.Functor.<$> (v Data.Aeson..: "context")
        Control.Applicative.<*> (v Data.Aeson..: "override")
        Control.Applicative.<*> (v Data.Aeson..: "description")
        Control.Applicative.<*> (v Data.Aeson..: "change_reason")
    



data UpdateContextOverrideRequestBuilderState = UpdateContextOverrideRequestBuilderState {
    contextBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.ContextIdentifier.ContextIdentifier,
    overrideBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value),
    descriptionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    change_reasonBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateContextOverrideRequestBuilderState
defaultBuilderState = UpdateContextOverrideRequestBuilderState {
    contextBuilderState = Data.Maybe.Nothing,
    overrideBuilderState = Data.Maybe.Nothing,
    descriptionBuilderState = Data.Maybe.Nothing,
    change_reasonBuilderState = Data.Maybe.Nothing
}

type UpdateContextOverrideRequestBuilder = Control.Monad.State.Strict.State UpdateContextOverrideRequestBuilderState

setContext :: Io.Superposition.Model.ContextIdentifier.ContextIdentifier -> UpdateContextOverrideRequestBuilder ()
setContext value =
   Control.Monad.State.Strict.modify (\s -> (s { contextBuilderState = Data.Maybe.Just value }))

setOverride :: Data.Map.Map Data.Text.Text Data.Aeson.Value -> UpdateContextOverrideRequestBuilder ()
setOverride value =
   Control.Monad.State.Strict.modify (\s -> (s { overrideBuilderState = Data.Maybe.Just value }))

setDescription :: Data.Maybe.Maybe Data.Text.Text -> UpdateContextOverrideRequestBuilder ()
setDescription value =
   Control.Monad.State.Strict.modify (\s -> (s { descriptionBuilderState = value }))

setChangeReason :: Data.Text.Text -> UpdateContextOverrideRequestBuilder ()
setChangeReason value =
   Control.Monad.State.Strict.modify (\s -> (s { change_reasonBuilderState = Data.Maybe.Just value }))

build :: UpdateContextOverrideRequestBuilder () -> Data.Either.Either Data.Text.Text UpdateContextOverrideRequest
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    context' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateContextOverrideRequest.UpdateContextOverrideRequest.context is a required property.") Data.Either.Right (contextBuilderState st)
    override' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateContextOverrideRequest.UpdateContextOverrideRequest.override is a required property.") Data.Either.Right (overrideBuilderState st)
    description' <- Data.Either.Right (descriptionBuilderState st)
    change_reason' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateContextOverrideRequest.UpdateContextOverrideRequest.change_reason is a required property.") Data.Either.Right (change_reasonBuilderState st)
    Data.Either.Right (UpdateContextOverrideRequest { 
        context = context',
        override = override',
        description = description',
        change_reason = change_reason'
    })



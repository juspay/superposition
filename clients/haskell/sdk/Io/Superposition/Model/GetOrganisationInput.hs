module Io.Superposition.Model.GetOrganisationInput (
    setId',
    build,
    GetOrganisationInputBuilder,
    GetOrganisationInput,
    id'
) where
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data GetOrganisationInput = GetOrganisationInput {
    id' :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetOrganisationInput where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetOrganisationInput

instance Data.Aeson.FromJSON GetOrganisationInput where
    parseJSON = Data.Aeson.withObject "GetOrganisationInput" $ \v -> GetOrganisationInput
        Data.Functor.<$> (v Data.Aeson..: "id")
    



data GetOrganisationInputBuilderState = GetOrganisationInputBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetOrganisationInputBuilderState
defaultBuilderState = GetOrganisationInputBuilderState {
    id'BuilderState = Data.Maybe.Nothing
}

type GetOrganisationInputBuilder = Control.Monad.State.Strict.State GetOrganisationInputBuilderState

setId' :: Data.Text.Text -> GetOrganisationInputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

build :: GetOrganisationInputBuilder () -> Data.Either.Either Data.Text.Text GetOrganisationInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetOrganisationInput.GetOrganisationInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    Data.Either.Right (GetOrganisationInput { 
        id' = id''
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetOrganisationInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Io.Superposition.Utility.setPath [
            "superposition",
            "organisations",
            Io.Superposition.Utility.serializeElement (id' self)
            ]
        
        
        


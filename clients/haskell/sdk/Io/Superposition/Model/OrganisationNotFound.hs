module Io.Superposition.Model.OrganisationNotFound (
    build,
    OrganisationNotFoundBuilder,
    OrganisationNotFound
) where
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types

data OrganisationNotFound = OrganisationNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON OrganisationNotFound where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody OrganisationNotFound

instance Data.Aeson.FromJSON OrganisationNotFound where
    parseJSON = Data.Aeson.withObject "OrganisationNotFound" $ \_ -> pure $ OrganisationNotFound



data OrganisationNotFoundBuilderState = OrganisationNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: OrganisationNotFoundBuilderState
defaultBuilderState = OrganisationNotFoundBuilderState {
}

type OrganisationNotFoundBuilder = Control.Monad.State.Strict.State OrganisationNotFoundBuilderState


build :: OrganisationNotFoundBuilder () -> Data.Either.Either Data.Text.Text OrganisationNotFound
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (OrganisationNotFound { 
    })


instance Io.Superposition.Utility.FromResponseParser OrganisationNotFound where
    expectedStatus = Network.HTTP.Types.status404
    responseParser = do
        
        
        pure $ OrganisationNotFound {
            
        }


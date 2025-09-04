module Io.Superposition.Model.TypeTemplatesNotFound (
    build,
    TypeTemplatesNotFoundBuilder,
    TypeTemplatesNotFound
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

data TypeTemplatesNotFound = TypeTemplatesNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TypeTemplatesNotFound where
    toJSON a = Data.Aeson.object [
        ]
    

instance Io.Superposition.Utility.SerializeBody TypeTemplatesNotFound

instance Data.Aeson.FromJSON TypeTemplatesNotFound where
    parseJSON = Data.Aeson.withObject "TypeTemplatesNotFound" $ \_ -> pure $ TypeTemplatesNotFound



data TypeTemplatesNotFoundBuilderState = TypeTemplatesNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TypeTemplatesNotFoundBuilderState
defaultBuilderState = TypeTemplatesNotFoundBuilderState {
}

type TypeTemplatesNotFoundBuilder = Control.Monad.State.Strict.State TypeTemplatesNotFoundBuilderState


build :: TypeTemplatesNotFoundBuilder () -> Data.Either.Either Data.Text.Text TypeTemplatesNotFound
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    Data.Either.Right (TypeTemplatesNotFound { 
    })


instance Io.Superposition.Utility.FromResponseParser TypeTemplatesNotFound where
    expectedStatus = Network.HTTP.Types.status404
    responseParser = do
        
        
        pure $ TypeTemplatesNotFound {
            
        }

